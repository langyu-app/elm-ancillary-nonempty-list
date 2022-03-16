module List.Nonempty.Ancillary exposing
    ( setAt, updateAt
    , maximum, maximumBy, maximumWith, minimum, minimumBy, minimumWith
    , indexedMaximum, indexedMaximumBy, indexedMaximumWith, indexedMinimum, indexedMinimumBy, indexedMinimumWith
    , combine, traverse
    , decodeArray, encodeArray, decodeObject, encodeObject
    , shuffle
    )

{-| The `List.Nonempty.Ancillary` module provides additional convenience
functions not found in `mgold/elm-nonempty-list` for dealing with non-empty
lists.


# Getting, Setting, and Updating

@docs setAt, updateAt


# Finding Extrema

Find minimum/maximum elements without `Maybe`s.

@docs maximum, maximumBy, maximumWith, minimum, minimumBy, minimumWith


# Finding Extrema With Indices

Find minimum/maximum elements and their indices without `Maybe`s.

@docs indexedMaximum, indexedMaximumBy, indexedMaximumWith, indexedMinimum, indexedMinimumBy, indexedMinimumWith


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

@docs shuffle

-}

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

-}
updateAt : Int -> (a -> a) -> Nonempty a -> Nonempty a
updateAt i f (Nonempty x xs) =
    let
        j : Int
        j =
            modBy (1 + List.length xs) i
    in
    if j == 0 then
        Nonempty (f x) xs

    else
        Nonempty x <| ListX.updateAt (j - 1) f xs


{-| Find the maximum element in a non-empty list.
-}
maximum : Nonempty comparable -> comparable
maximum (Nonempty x xs) =
    List.foldl max x xs


{-| Given a function to map a type to a comparable type, find the **first**
maximum element in a non-empty list.
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
-}
minimum : Nonempty comparable -> comparable
minimum (Nonempty x xs) =
    List.foldl min x xs


{-| Given a function to map a type to a comparable type, find the **first**
minimum element in a non-empty list.
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
-}
decodeArray : Decoder a -> Decoder (Nonempty a)
decodeArray d =
    Decode.oneOrMore Nonempty d


{-| Turn a non-empty list into a JSON array.
-}
encodeArray : (a -> Encode.Value) -> Nonempty a -> Encode.Value
encodeArray e =
    Encode.list e << NE.toList


{-| Decode a non-empty list from a JSON object of the form:

    { "head": x1
    , "tail": [x2, x3]
    }

-}
decodeObject : Decoder a -> Decoder (Nonempty a)
decodeObject d =
    Decode.map2 Nonempty
        (Decode.field "head" d)
        (Decode.field "tail" (Decode.list d))


{-| Encode a non-empty list into a JSON object of the form:

    { "head": x1
    , "tail": [x2, x3]
    }

-}
encodeObject : (a -> Encode.Value) -> Nonempty a -> Encode.Value
encodeObject e (Nonempty x xs) =
    Encode.object [ ( "head", e x ), ( "tail", Encode.list e xs ) ]


{-| Shuffle the non-empty list. This of course has no effect on a list of one
element.
-}
shuffle : Nonempty a -> Generator (Nonempty a)
shuffle l =
    NE.toList l
        |> Random.List.shuffle
        |> Random.map (Maybe.withDefault l << NE.fromList)
