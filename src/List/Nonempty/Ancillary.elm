module List.Nonempty.Ancillary exposing
    ( setAt, updateAt
    , appendList, prependList, initialize
    , foldr, foldr1, indexedFoldl, indexedFoldr
    , unique, uniqueBy, allDifferent, allDifferentBy
    , maximum, maximumBy, maximumWith, minimum, minimumBy, minimumWith
    , indexedMaximum, indexedMaximumBy, indexedMaximumWith, indexedMinimum, indexedMinimumBy, indexedMinimumWith
    , find, elemIndex, elemIndices, findIndex, findIndices, findMap, count
    , combine, traverse
    , decodeArray, encodeArray, decodeObject, encodeObject
    , generator, shuffle, sequenceGenerators
    )

{-| The `List.Nonempty.Ancillary` module provides additional convenience
functions not found in `mgold/elm-nonempty-list` for dealing with non-empty
lists.


# Getting, Setting, and Updating

@docs setAt, updateAt


# Building

@docs appendList, prependList, initialize


# Folds

@docs foldr, foldr1, indexedFoldl, indexedFoldr


# Uniqueness

@docs unique, uniqueBy, allDifferent, allDifferentBy


# Finding Extrema

Find minimum/maximum elements without `Maybe`s.

@docs maximum, maximumBy, maximumWith, minimum, minimumBy, minimumWith


# Finding Extrema With Indices

Find minimum/maximum elements and their indices without `Maybe`s.

@docs indexedMaximum, indexedMaximumBy, indexedMaximumWith, indexedMinimum, indexedMinimumBy, indexedMinimumWith


# Searching

@docs find, elemIndex, elemIndices, findIndex, findIndices, findMap, count


# `Maybe`s

@docs combine, traverse


# JSON Decoders/Encoders

The module provides two sets of JSON decoder/encodes. In the first,
`decodeArray`/`encodeArray`, the non-empty list is represented as a standard
JSON array. Decoding an empty array will fail, and encoding to an empty array
will never happen.

In the second, `decodeObject`/`encodeObject`, the non-empty list is explicitly
represented as an object with two keys: `"head"` (containing the first element)
and `"tail"`, containing a (possibly empty) array of the rest of the elements:

    { "head": x1
    , "tail": [x2, x3]
    }

@docs decodeArray, encodeArray, decodeObject, encodeObject


# Random

@docs generator, shuffle, sequenceGenerators

-}

import Basics.Extra exposing (safeModBy)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Extra as ListX
import List.Nonempty as NE exposing (Nonempty(..))
import Random exposing (Generator)
import Random.List


{-| Given an index and a value, replace the element at that index with the given
value. Indices are modulus the length of the list, so out-of-range errors
cannot occur; this means that negative indices are supported, e.g. -1 to set the
last element. This is consistent with the behavior of `List.Nonempty.get`

Note that this is _not_ particularly efficient (iterating over the entire list
multiple times) and should probably not be used for very large lists. `Array`s
are of course preferable in such cases.

    import List.Nonempty exposing (Nonempty(..))

    setAt 1 "er" <| Nonempty "foo" [ "bar", "baz" ]
    --> Nonempty "foo" [ "er", "baz" ]

    setAt 6 "yi" <| Nonempty "foo" [ "bar", "baz" ]
    --> Nonempty "yi" [ "bar", "baz" ]

    setAt -1 "san" <| Nonempty "foo" [ "bar", "baz" ]
    --> Nonempty "foo" [ "bar", "san" ]

-}
setAt : Int -> a -> Nonempty a -> Nonempty a
setAt i x =
    updateAt i (always x)


{-| Given an index and an update function, replace the value at that index
by calling the update function. Indices are modulus the length of the list, so
out-of-range errors cannot occur; this means that negative indices are
supported, e.g. -1 to update the last element. This is consistent with the
behavior of `List.Nonempty.get`

Note that this is _not_ particularly efficient (iterating over the entire list
multiple times) and should probably not be used for very large lists. `Array`s
are of course preferable in such cases.

    import List.Nonempty exposing (Nonempty(..))

    updateAt 1 ((+) 1) <| Nonempty 1 [ 1, 1 ]
    --> Nonempty 1 [ 2, 1 ]

    updateAt 6 ((+) 1) <| Nonempty 1 [ 1, 1 ]
    --> Nonempty 2 [ 1, 1 ]

    updateAt -1 ((+) 1) <| Nonempty 1 [ 1, 1 ]
    --> Nonempty 1 [ 1, 2 ]

-}
updateAt : Int -> (a -> a) -> Nonempty a -> Nonempty a
updateAt i f (Nonempty x xs) =
    case safeModBy (1 + List.length xs) i of
        Just 0 ->
            Nonempty (f x) xs

        Just j ->
            Nonempty x <| ListX.updateAt (j - 1) f xs

        Nothing ->
            Nonempty x xs


{-| Append a list to the end of a non-empty list.

    import List.Nonempty exposing (Nonempty(..))

    appendList (Nonempty 1 [ 2 ]) [ 3, 4 ]
    --> Nonempty 1 [ 2, 3, 4 ]

    appendList (Nonempty 1 [ 2 ]) []
    --> Nonempty 1 [ 2 ]

-}
appendList : Nonempty a -> List a -> Nonempty a
appendList (Nonempty x xs) ys =
    Nonempty x (xs ++ ys)


{-| Prepend a list to the beginning of a non-empty list.

    import List.Nonempty exposing (Nonempty(..))

    prependList [ 1, 2 ] <| Nonempty 3 [ 4 ]
    --> Nonempty 1 [ 2, 3, 4 ]

    prependList [] <| Nonempty 1 [ 2 ]
    --> Nonempty 1 [ 2 ]

-}
prependList : List a -> Nonempty a -> Nonempty a
prependList xs (Nonempty y ys) =
    case xs of
        [] ->
            Nonempty y ys

        x :: xs_ ->
            Nonempty x (xs_ ++ y :: ys)


{-| Initialize a list of a given length by calling a function with each index.
The resulting list will always have at least 1 item, even if called with a
negative or zero length.

    import List.Nonempty exposing (Nonempty(..))

    initialize 1 (\i -> i)
    --> Nonempty 0 []

    initialize 3 (\i -> i * 2)
    --> Nonempty 0 [ 2, 4 ]

    initialize 0 (\i -> i + 1)
    --> Nonempty 1 []

    initialize -1 (\i -> i + 1)
    --> Nonempty 1 []

-}
initialize : Int -> (Int -> a) -> Nonempty a
initialize n f =
    let
        step : Int -> List a -> List a
        step i acc =
            if i <= 0 then
                acc

            else
                step (i - 1) (f i :: acc)
    in
    Nonempty (f 0) <| step (n - 1) []


{-| Reduce a non-empty list from the right.

    import List.Nonempty exposing (Nonempty(..))

    foldr (+) 0 <| Nonempty 1 [ 2, 3 ]
    --> 6

    foldr (::) [] <| Nonempty 1 [ 2, 3 ]
    --> [ 1, 2, 3 ]

-}
foldr : (a -> b -> b) -> b -> Nonempty a -> b
foldr step init (Nonempty x xs) =
    List.foldr step init <| x :: xs


{-| Reduce a non-empty list from the right, treating the last element of the list as the starting value.

    import List.Nonempty exposing (Nonempty(..))

    foldr1 (-) <| Nonempty 1 [ 2, 3, 4 ]
    --> -2

    foldr1 (++) <| Nonempty "a" [ "b", "c" ]
    --> "abc"

-}
foldr1 : (a -> a -> a) -> Nonempty a -> a
foldr1 step =
    NE.foldl1 step << NE.reverse


{-| `foldl` that also passes the index of the current element to the step
function.

    import List.Nonempty exposing (Nonempty(..))

    indexedFoldl (\i x acc -> String.fromInt i ++ x ++ acc) "" <| Nonempty "a" [ "b", "c" ]
    --> "2c1b0a"

-}
indexedFoldl : (Int -> a -> b -> b) -> b -> Nonempty a -> b
indexedFoldl f init =
    let
        step : a -> ( Int, b ) -> ( Int, b )
        step x ( i, acc ) =
            ( i + 1, f i x acc )
    in
    Tuple.second << NE.foldl step ( 0, init )


{-| `foldr` that also passes the index of the current element to the step
function. Note that this isn't particularly efficient, as it traverses the list
multiple times.

    import List.Nonempty exposing (Nonempty(..))

    indexedFoldr (\i x acc -> String.fromInt i ++ x ++ acc) "" <| Nonempty "a" [ "b", "c" ]
    --> "0a1b2c"

-}
indexedFoldr : (Int -> a -> b -> b) -> b -> Nonempty a -> b
indexedFoldr f init xs =
    let
        step : a -> ( Int, b ) -> ( Int, b )
        step x ( i, acc ) =
            ( i - 1, f i x acc )
    in
    Tuple.second <| foldr step ( NE.length xs - 1, init ) xs


{-| Remove all duplicate values from the non-empty list, keeping only the first
instance of each element that appears more than once.

    import List.Nonempty exposing (Nonempty(..))

    unique <| Nonempty 0  [ 1, 1, 0, 1 ]
    --> Nonempty 0 [ 1 ]

-}
unique : Nonempty a -> Nonempty a
unique =
    uniqueBy identity


{-| Remove all duplicate values from the non-empty list, where uniqueness is
determined first by applying a function, keeping only the first instance of each
element that appears more than once.

    import List.Nonempty exposing (Nonempty(..))

    uniqueBy (String.left 1) <| Nonempty "foo"  [ "bar", "baz", "fun", "boil" ]
    --> Nonempty "foo" [ "bar" ]

Note that this only applies the function once for each element, in case it is an
expensive operation.

-}
uniqueBy : (a -> b) -> Nonempty a -> Nonempty a
uniqueBy f (Nonempty x xs) =
    let
        go : List b -> List a -> List a -> List a
        go existing remaining accumulator =
            case remaining of
                [] ->
                    List.reverse accumulator

                y :: ys ->
                    let
                        fY : b
                        fY =
                            f y
                    in
                    if List.member fY existing then
                        go existing ys accumulator

                    else
                        go (fY :: existing) ys (y :: accumulator)
    in
    go [ f x ] xs []
        |> Nonempty x


{-| Indicate whether or not the non-empty list has duplicate values.

    import List.Nonempty exposing (Nonempty(..))

    allDifferent <| Nonempty 0 [ 1, 1, 0, 1 ]
    --> False

    allDifferent <| Nonempty 0 [ 1, 2]
    --> True

-}
allDifferent : Nonempty a -> Bool
allDifferent =
    allDifferentBy identity


{-| Indicate whether or not the non-empty list has duplicate values after
transformation by some function.

    import List.Nonempty exposing (Nonempty(..))

    allDifferentBy (String.left 1) <| Nonempty "foo" [ "bar", "baz" ]
    --> False

    allDifferentBy (String.left 1) <| Nonempty "far" [ "bar", "car" ]
    --> True

Note that this only applies the function once for each element, in case it is an
expensive operation.

-}
allDifferentBy : (a -> b) -> Nonempty a -> Bool
allDifferentBy f list =
    let
        go : Nonempty a -> Nonempty a -> Bool
        go l1 l2 =
            -- Compare lengths recursively so it can short circuit
            case ( l1, l2 ) of
                ( Nonempty _ [], Nonempty _ [] ) ->
                    True

                ( Nonempty _ (x :: xs), Nonempty _ (y :: ys) ) ->
                    go (Nonempty x xs) (Nonempty y ys)

                _ ->
                    False
    in
    uniqueBy f list
        |> go list


{-| Find the maximum element in a non-empty list.

    import List.Nonempty exposing (Nonempty(..))

    maximum <| Nonempty 1 [ 3, 2 ]
    --> 3

-}
maximum : Nonempty comparable -> comparable
maximum (Nonempty x xs) =
    List.foldl max x xs


{-| Given a function to map a type to a comparable type, find the **first**
maximum element in a non-empty list.

    import List.Nonempty exposing (Nonempty(..))

    maximumBy (\i -> i * i) <| Nonempty 1 [ -3, 3 ]
    --> -3

-}
maximumBy : (a -> comparable) -> Nonempty a -> a
maximumBy f (Nonempty l ls) =
    let
        step : a -> ( a, comparable ) -> ( a, comparable )
        step x (( _, fY ) as acc) =
            let
                fX : comparable
                fX =
                    f x
            in
            if fX > fY then
                ( x, fX )

            else
                acc
    in
    Tuple.first <| List.foldl step ( l, f l ) ls


{-| Given a comparison function, find the **first** maximum element in a
non-empty list.

    import List.Nonempty exposing (Nonempty(..))

    Nonempty { id = 0, val = 1 } [ { id = 1, val = 3 }, { id = 2, val = 0 } ]
        |> maximumWith (\a b -> compare a.val b.val)
    --> { id = 1, val = 3 }

-}
maximumWith : (a -> a -> Order) -> Nonempty a -> a
maximumWith f (Nonempty l ls) =
    let
        step : a -> a -> a
        step x acc =
            case f x acc of
                GT ->
                    x

                _ ->
                    acc
    in
    List.foldl step l ls


{-| Find the minimum element in a non-empty list.

    import List.Nonempty exposing (Nonempty(..))

    minimum <| Nonempty 1 [ 3, 2 ]
    --> 1

-}
minimum : Nonempty comparable -> comparable
minimum (Nonempty x xs) =
    List.foldl min x xs


{-| Given a function to map a type to a comparable type, find the **first**
minimum element in a non-empty list.

    import List.Nonempty exposing (Nonempty(..))

    minimumBy (\i -> i * i) <| Nonempty 1 [ -1, 2 ]
    --> 1

-}
minimumBy : (a -> comparable) -> Nonempty a -> a
minimumBy f (Nonempty l ls) =
    let
        step : a -> ( a, comparable ) -> ( a, comparable )
        step x (( _, fY ) as acc) =
            let
                fX : comparable
                fX =
                    f x
            in
            if fX < fY then
                ( x, fX )

            else
                acc
    in
    Tuple.first <| List.foldl step ( l, f l ) ls


{-| Given a comparison function, find the **first** minimum element in a
non-empty list.

    import List.Nonempty exposing (Nonempty(..))

    Nonempty { id = 0, val = 1 } [ { id = 1, val = 3 }, { id = 2, val = 0 } ]
        |> minimumWith (\a b -> compare a.val b.val)
    --> { id = 2, val = 0 }

-}
minimumWith : (a -> a -> Order) -> Nonempty a -> a
minimumWith f (Nonempty l ls) =
    let
        step : a -> a -> a
        step x acc =
            case f x acc of
                LT ->
                    x

                _ ->
                    acc
    in
    List.foldl step l ls


{-| Find the **first** maximum element in a non-empty list and its index.

    import List.Nonempty exposing (Nonempty(..))

    indexedMaximum <| Nonempty 1 [ 3, 2, 3 ]
    --> ( 1, 3 )

-}
indexedMaximum : Nonempty comparable -> ( Int, comparable )
indexedMaximum (Nonempty l ls) =
    let
        step : Int -> comparable -> ( Int, comparable ) -> ( Int, comparable )
        step i x (( _, y ) as acc) =
            if x > y then
                -- Increment index by 1, since folding over tail
                ( i + 1, x )

            else
                acc
    in
    ListX.indexedFoldl step ( 0, l ) ls


{-| Given a function to map a type to a comparable type, find the **first**
maximum element in a non-empty list and its index.

    import List.Nonempty exposing (Nonempty(..))

    indexedMaximumBy (\i -> i * i) <| Nonempty 1 [ -3, 3 ]
    --> ( 1, -3 )

-}
indexedMaximumBy : (a -> comparable) -> Nonempty a -> ( Int, a )
indexedMaximumBy f (Nonempty l ls) =
    let
        step : Int -> a -> ( ( Int, a ), comparable ) -> ( ( Int, a ), comparable )
        step i x (( _, fY ) as acc) =
            let
                fX : comparable
                fX =
                    f x
            in
            if fX > fY then
                -- Increment index by 1, since folding over tail
                ( ( i + 1, x ), fX )

            else
                acc
    in
    Tuple.first <| ListX.indexedFoldl step ( ( 0, l ), f l ) ls


{-| Given a comparison function, find the **first** maximum element in a
non-empty list and its index.

    import List.Nonempty exposing (Nonempty(..))

    Nonempty { id = 0, val = 1 } [ { id = 1, val = 3 }, { id = 2, val = 3 } ]
        |> indexedMaximumWith (\a b -> compare a.val b.val)
    --> ( 1, { id = 1, val = 3 } )

-}
indexedMaximumWith : (a -> a -> Order) -> Nonempty a -> ( Int, a )
indexedMaximumWith f (Nonempty l ls) =
    let
        step : Int -> a -> ( Int, a ) -> ( Int, a )
        step i x (( _, y ) as acc) =
            case f x y of
                GT ->
                    ( i + 1, x )

                _ ->
                    acc
    in
    ListX.indexedFoldl step ( 0, l ) ls


{-| Find the **first** minimum element in a non-empty list and its index.

    import List.Nonempty exposing (Nonempty(..))

    indexedMinimum <| Nonempty 2 [ 2, 1, 1 ]
    --> ( 2, 1 )

-}
indexedMinimum : Nonempty comparable -> ( Int, comparable )
indexedMinimum (Nonempty l ls) =
    let
        step : Int -> comparable -> ( Int, comparable ) -> ( Int, comparable )
        step i x (( _, y ) as acc) =
            if x < y then
                -- Increment index by 1, since folding over tail
                ( i + 1, x )

            else
                acc
    in
    ListX.indexedFoldl step ( 0, l ) ls


{-| Given a function to map a type to a comparable type, find the **first**
minimum element in a non-empty list and its index.

    import List.Nonempty exposing (Nonempty(..))

    indexedMinimumBy (\i -> i * i) <| Nonempty 2 [ -1, 1 ]
    --> ( 1, -1 )

-}
indexedMinimumBy : (a -> comparable) -> Nonempty a -> ( Int, a )
indexedMinimumBy f (Nonempty l ls) =
    let
        step : Int -> a -> ( ( Int, a ), comparable ) -> ( ( Int, a ), comparable )
        step i x (( _, fY ) as acc) =
            let
                fX : comparable
                fX =
                    f x
            in
            if fX < fY then
                -- Increment index by 1, since folding over tail
                ( ( i + 1, x ), fX )

            else
                acc
    in
    Tuple.first <| ListX.indexedFoldl step ( ( 0, l ), f l ) ls


{-| Given a comparison function, find the **first** minimum element in a
non-empty list and its index.

    import List.Nonempty exposing (Nonempty(..))

    Nonempty { id = 0, val = 1 } [ { id = 1, val = 3 }, { id = 2, val = 1 } ]
        |> indexedMinimumWith (\a b -> compare a.val b.val)
    --> ( 0, { id = 0, val = 1 } )

-}
indexedMinimumWith : (a -> a -> Order) -> Nonempty a -> ( Int, a )
indexedMinimumWith f (Nonempty l ls) =
    let
        step : Int -> a -> ( Int, a ) -> ( Int, a )
        step i x (( _, y ) as acc) =
            case f x y of
                LT ->
                    ( i + 1, x )

                _ ->
                    acc
    in
    ListX.indexedFoldl step ( 0, l ) ls


{-| Find the first element that satisfies a predicate and return `Just` that
element, or if none match, return `Nothing`.

    import List.Nonempty exposing (Nonempty(..))

    find (\i -> i > 3) <| Nonempty 2 [ 4, 6, 8 ]
    --> Just 4

    find (\i -> i > 3) <| Nonempty 0 [ 1, 2, 3 ]
    --> Nothing

-}
find : (a -> Bool) -> Nonempty a -> Maybe a
find pred (Nonempty first rest) =
    let
        go : List a -> Maybe a
        go xs =
            case xs of
                [] ->
                    Nothing

                x :: xs_ ->
                    if pred x then
                        Just x

                    else
                        go xs_
    in
    go (first :: rest)


{-| Return `Just` the index (starting from 0) of the first instance of the
element. If the element does not exist in the list, return `Nothing`.

    import List.Nonempty exposing (Nonempty(..))

    elemIndex 1 <| Nonempty 1 [ 2, 3 ]
    --> Just 0

    elemIndex 4 <| Nonempty 1 [ 2, 3 ]
    --> Nothing

    elemIndex 1 <| Nonempty 1 [ 1, 7 ]
    --> Just 0

-}
elemIndex : a -> Nonempty a -> Maybe Int
elemIndex x =
    findIndex ((==) x)


{-| Return a (possibly empty) list of all indices (starting from 0) at which the
element occurs.

    import List.Nonempty exposing (Nonempty(..))

    elemIndices 1 <| Nonempty 1 [ 2, 3 ]
    --> [ 0 ]

    elemIndices 4 <| Nonempty 1 [ 2, 3 ]
    --> []

    elemIndices 1 <| Nonempty 1 [ 1, 7 ]
    --> [ 0, 1 ]

-}
elemIndices : a -> Nonempty a -> List Int
elemIndices x =
    findIndices ((==) x)


{-| Given a predicate and a nonempty list, return `Just` the index (starting
from 0) of the first element that satisfies the predicate. If no element in the
list satisfies the predicate, return `Nothing`.

    import List.Nonempty exposing (Nonempty(..))

    findIndex (\i -> i == 7) <| Nonempty 1 [ 1, 7 ]
    --> Just 2

    findIndex (\i -> i < 1) <| Nonempty 1 [ 3, 5 ]
    --> Nothing

    findIndex (\i -> i > 1) <| Nonempty 1 [ 2, 3 ]
    --> Just 1

-}
findIndex : (a -> Bool) -> Nonempty a -> Maybe Int
findIndex p (Nonempty head tail) =
    let
        go : Int -> List a -> Maybe Int
        go i l =
            case l of
                [] ->
                    Nothing

                x :: xs ->
                    if p x then
                        Just i

                    else
                        go (i + 1) xs
    in
    go 0 <| head :: tail


{-| Given a predicate and a nonempty list, return a (possibly empty) list of all
indices (starting from 0) at which the element satisfies the predicate.

    import List.Nonempty exposing (Nonempty(..))

    findIndices (\i -> i == 7) <| Nonempty 1 [ 1, 7 ]
    --> [ 2 ]

    findIndices (\i -> i < 1) <| Nonempty 1 [ 3, 5 ]
    --> []

    findIndices (\i -> i > 1) <| Nonempty 1 [ 2, 3 ]
    --> [ 1, 2 ]

-}
findIndices : (a -> Bool) -> Nonempty a -> List Int
findIndices p =
    let
        step : Int -> a -> List Int -> List Int
        step i x acc =
            if p x then
                i :: acc

            else
                acc
    in
    indexedFoldr step []


{-| Apply a function that may succeed (return a `Just` value) to values in a
non-empty list, returning the result of the first successful match. If none
match, then return Nothing.

    import List.Nonempty exposing (Nonempty(..))

    findMap String.toInt <| Nonempty "a" [ "b", "3" ]
    --> Just 3

    findMap String.toInt <| Nonempty "a" [ "b", "c" ]
    --> Nothing

-}
findMap : (a -> Maybe b) -> Nonempty a -> Maybe b
findMap f (Nonempty x xs) =
    let
        go : List a -> Maybe b
        go list =
            case list of
                [] ->
                    Nothing

                a :: tail ->
                    case f a of
                        Just b ->
                            Just b

                        Nothing ->
                            go tail
    in
    go (x :: xs)


{-| Return the number of elements in the list that satisfy a given predicate.

    import List.Nonempty exposing (Nonempty(..))

    count ((==) Nothing << String.toInt) <| Nonempty "1" [ "yi", "2", "er", "3", "san" ]
    --> 3

-}
count : (a -> Bool) -> Nonempty a -> Int
count pred =
    NE.foldl
        (\x acc ->
            if pred x then
                acc + 1

            else
                acc
        )
        0


{-| If every `Maybe` in the non-empty list is `Just a`, then return a list of
all the unwrapped values. If one or more elements are `Nothing`, then the
entire output will be `Nothing`.

    import List.Nonempty exposing (Nonempty(..))

    combine (Nonempty (Just 1) [ Just 2, Just 3 ])
    --> Just (Nonempty 1 [ 2, 3 ])

    combine (Nonempty (Just 1) [ Nothing, Just 3 ])
    --> Nothing

-}
combine : Nonempty (Maybe a) -> Maybe (Nonempty a)
combine l =
    List.foldr (Maybe.map2 (::))
        (Just [])
        (NE.tail l)
        |> Maybe.map2 Nonempty (NE.head l)


{-| Map a function over every element in the non-empty list. If every function
call returns `Just a`, then return a list of all the values. If one or more
function call returns `Nothing`, then the entire output will be `Nothing`.

    import List.Nonempty exposing (Nonempty(..))

    traverse List.head (Nonempty [ 1 ] [ [2], [ 3, 4 ] ])
    --> Just (Nonempty 1 [ 2, 3 ])

    traverse List.head (Nonempty [ 1 ] [ [], [ 3, 4 ] ])
    --> Nothing

-}
traverse : (a -> Maybe b) -> Nonempty a -> Maybe (Nonempty b)
traverse f xs =
    List.foldr (\x -> Maybe.map2 (::) (f x))
        (Just [])
        (NE.tail xs)
        |> Maybe.map2 Nonempty (f <| NE.head xs)


{-| Decode a non-empty list from a JSON array, failing if it is empty.

    import List.Nonempty exposing (Nonempty(..))
    import Json.Decode as Decode

    Decode.decodeString (decodeArray Decode.int) "[1,2,3]"
    --> Ok (Nonempty 1 [ 2, 3 ])
    Decode.decodeString (decodeArray Decode.int) "[]"
        |> Result.toMaybe
    --> Nothing

-}
decodeArray : Decoder a -> Decoder (Nonempty a)
decodeArray d =
    Decode.oneOrMore Nonempty d


{-| Turn a non-empty list into a JSON array.

    import List.Nonempty exposing (Nonempty(..))
    import Json.Encode as Encode

    Encode.encode 0 (encodeArray Encode.int <| Nonempty 1 [ 2, 3 ])
    --> "[1,2,3]"

-}
encodeArray : (a -> Encode.Value) -> Nonempty a -> Encode.Value
encodeArray e =
    Encode.list e << NE.toList


{-| Decode a non-empty list from a JSON object of the form:

    { "head": x1
    , "tail": [x2, x3]
    }


    import List.Nonempty exposing (Nonempty(..))
    import Json.Decode as Decode

    Decode.decodeString (decodeObject Decode.int) "{ \"head\": 1, \"tail\": [2,3] }"
    --> Ok (Nonempty 1 [ 2, 3 ])

-}
decodeObject : Decoder a -> Decoder (Nonempty a)
decodeObject d =
    Decode.map2 Nonempty
        (Decode.field "head" d)
        (Decode.field "tail" <| Decode.list d)


{-| Encode a non-empty list into a JSON object of the form:

    { "head": x1
    , "tail": [x2, x3]
    }

    import List.Nonempty exposing (Nonempty(..))
    import Json.Encode as Encode

    Encode.encode 0 (encodeObject Encode.int <| Nonempty 1 [ 2, 3 ])
    --> "{\"head\":1,\"tail\":[2,3]}"

-}
encodeObject : (a -> Encode.Value) -> Nonempty a -> Encode.Value
encodeObject e (Nonempty x xs) =
    Encode.object [ ( "head", e x ), ( "tail", Encode.list e xs ) ]


{-| Generate a non-empty list of a specified length. If the length is less than
1, the resultant list will be of length 1.

For instance, `generator 1 g`, `generator 0 g` and `generator -1 g` will all
produce a non-empty list of length 1, filled with a value generated by `g`.

-}
generator : Int -> Generator a -> Generator (Nonempty a)
generator n g =
    Random.list (n - 1) g
        |> Random.map2 Nonempty g


{-| Shuffle the non-empty list. This of course has no effect on a list of one
element.
-}
shuffle : Nonempty a -> Generator (Nonempty a)
shuffle l =
    NE.toList l
        |> Random.List.shuffle
        |> Random.map (Maybe.withDefault l << NE.fromList)


{-| Given a non-empty list random generators, turn them into a generator that
returns a non-empty list.
-}
sequenceGenerators : Nonempty (Generator a) -> Generator (Nonempty a)
sequenceGenerators =
    NE.reverse
        >> (\(Nonempty x xs) -> List.foldl (Random.map2 NE.cons) (Random.map NE.singleton x) xs)
