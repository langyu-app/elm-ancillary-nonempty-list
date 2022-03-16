module ListNonemptyAncillaryTest exposing (all)

import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra as ListX
import List.Nonempty as NE exposing (Nonempty(..))
import List.Nonempty.Ancillary
    exposing
        ( combine
        , decodeArray
        , decodeObject
        , encodeArray
        , encodeObject
        , indexedMaximum
        , indexedMaximumBy
        , indexedMaximumWith
        , indexedMinimum
        , indexedMinimumBy
        , indexedMinimumWith
        , maximum
        , maximumBy
        , maximumWith
        , minimum
        , minimumBy
        , minimumWith
        , setAt
        , shuffle
        , traverse
        , updateAt
        )
import Random
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)
import Test.Extra as TestX exposing (DecoderExpectation(..))


{-| All tests.
-}
all : Test
all =
    describe "List.Nonempty.Ancillary"
        [ extremaSuite
        , extremaWithIndicesSuite
        , getSetUpdateSuite
        , jsonSuite
        , maybeSuite
        , randomSuite
        ]


{-| Fuzz a non-empty list full of a given type.
-}
fuzzNonempty : Fuzzer a -> Fuzzer (Nonempty a)
fuzzNonempty f =
    Fuzz.map2 Nonempty f (Fuzz.list f)


{-| Fuzz a record, used for testing maximum/minimum functions.
-}
fuzzRecord : Fuzzer { a : String, b : Int }
fuzzRecord =
    Fuzz.map2 (\a b -> { a = a, b = b }) Fuzz.string Fuzz.int


{-| Test suite for `List.Nonempty.Ancillary` for getting/setting/updating.
-}
getSetUpdateSuite : Test
getSetUpdateSuite =
    describe "Getting, Setting, and Updating"
        [ describe "setAt"
            [ fuzz3 Fuzz.int Fuzz.string (fuzzNonempty Fuzz.string) "getting should always return the set value" <|
                \i x xs ->
                    setAt i x xs
                        |> NE.get i
                        |> Expect.equal x
            , fuzz3 Fuzz.int Fuzz.string (fuzzNonempty Fuzz.string) "the rest of the list should be unaffected" <|
                \i x xs ->
                    let
                        j : Int
                        j =
                            modBy (NE.length xs) i
                    in
                    setAt i x xs
                        |> NE.indexedMap Tuple.pair
                        |> NE.map2
                            (\orig ( i_, new ) ->
                                if i_ /= j then
                                    orig == new

                                else
                                    new == x
                            )
                            xs
                        |> NE.all identity
                        |> Expect.true "list should be unchanged"
            , fuzz3 Fuzz.int Fuzz.string (fuzzNonempty Fuzz.string) "setting should be reversible" <|
                \i x xs ->
                    setAt i x xs
                        |> setAt i (NE.get i xs)
                        |> NE.toList
                        |> Expect.equalLists (NE.toList xs)
            ]
        , describe "updateAt"
            [ fuzz3 Fuzz.int Fuzz.int (fuzzNonempty Fuzz.int) "getting should always return the updated value" <|
                \i add xs ->
                    updateAt i ((+) add) xs
                        |> NE.get i
                        |> Expect.equal (NE.get i xs + add)
            , fuzz3 Fuzz.int Fuzz.int (fuzzNonempty Fuzz.int) "the rest of the list should be unaffected" <|
                \i add xs ->
                    let
                        j : Int
                        j =
                            modBy (NE.length xs) i
                    in
                    updateAt i ((+) add) xs
                        |> NE.indexedMap Tuple.pair
                        |> NE.map2
                            (\orig ( i_, new ) ->
                                if i_ /= j then
                                    orig == new

                                else
                                    new == NE.get i xs + add
                            )
                            xs
                        |> NE.all identity
                        |> Expect.true "list should be unchanged"
            , fuzz3 Fuzz.int Fuzz.int (fuzzNonempty Fuzz.int) "updating should be reversible" <|
                \i add xs ->
                    updateAt i ((+) add) xs
                        |> updateAt i (\e -> e - add)
                        |> NE.toList
                        |> Expect.equalLists (NE.toList xs)
            ]
        ]


{-| Test suite for `List.Nonempty.Ancillary` for finding extrema.
-}
extremaSuite : Test
extremaSuite =
    describe "Finding Extrema"
        [ describe "maximum"
            [ fuzz (fuzzNonempty Fuzz.string) "should be equal to maximum of list" <|
                \l ->
                    Expect.equal (Just <| maximum l) (List.maximum <| NE.toList l)
            ]
        , describe "maximumBy"
            [ fuzz (fuzzNonempty fuzzRecord) "should be equal to maximum of list by a" <|
                \l ->
                    NE.toList l
                        |> ListX.maximumBy .a
                        |> Expect.equal (Just <| maximumBy .a l)
            , fuzz (fuzzNonempty fuzzRecord) "should be equal to maximum of list by b" <|
                \l ->
                    NE.toList l
                        |> ListX.maximumBy .b
                        |> Expect.equal (Just <| maximumBy .b l)
            , test "returns first result" <|
                \() ->
                    Nonempty { a = 2, b = 5 } [ { a = 4, b = 1 }, { a = 4, b = 2 } ]
                        |> maximumBy .a
                        |> Expect.equal { a = 4, b = 1 }
            ]
        , describe "maximumWith"
            [ fuzz (fuzzNonempty fuzzRecord) "should be equal to maximum of list" <|
                \l ->
                    let
                        f : { a : comparable, b : a } -> { a : comparable, b : a } -> Order
                        f x y =
                            compare x.a y.a
                    in
                    Expect.equal (Just <| maximumWith f l) (ListX.maximumWith f <| NE.toList l)
            , test "returns first result" <|
                \() ->
                    let
                        f : { a : comparable, b : a } -> { a : comparable, b : a } -> Order
                        f x y =
                            compare x.a y.a
                    in
                    Expect.equal { a = 4, b = 1 } <| maximumWith f (Nonempty { a = 2, b = 5 } [ { a = 4, b = 1 }, { a = 4, b = 2 } ])
            ]
        , describe "minimum"
            [ fuzz (fuzzNonempty Fuzz.string) "should be equal to minimum of list" <|
                \l ->
                    Expect.equal (Just <| minimum l) (List.minimum <| NE.toList l)
            ]
        , describe "minimumBy"
            [ fuzz (fuzzNonempty fuzzRecord) "should be equal to minimum of list by a" <|
                \l ->
                    NE.toList l
                        |> ListX.minimumBy .a
                        |> Expect.equal (Just <| minimumBy .a l)
            , fuzz (fuzzNonempty fuzzRecord) "should be equal to minimum of list by b" <|
                \l ->
                    NE.toList l
                        |> ListX.minimumBy .b
                        |> Expect.equal (Just <| minimumBy .b l)
            , test "returns first result" <|
                \() ->
                    Nonempty { a = 2, b = 5 } [ { a = 4, b = 1 }, { a = 2, b = 2 } ]
                        |> minimumBy .a
                        |> Expect.equal { a = 2, b = 5 }
            ]
        , describe "minimumWith"
            [ fuzz (fuzzNonempty fuzzRecord) "should be equal to minimum of list" <|
                \l ->
                    let
                        f : { a : comparable, b : a } -> { a : comparable, b : a } -> Order
                        f x y =
                            compare x.a y.a
                    in
                    Expect.equal (Just <| minimumWith f l) (ListX.minimumWith f <| NE.toList l)
            , test "returns first result" <|
                \() ->
                    let
                        f : { a : comparable, b : a } -> { a : comparable, b : a } -> Order
                        f x y =
                            compare x.a y.a
                    in
                    Expect.equal { a = 2, b = 5 } <| minimumWith f (Nonempty { a = 2, b = 5 } [ { a = 4, b = 1 }, { a = 2, b = 2 } ])
            ]
        ]


{-| Test suite for `List.Nonempty.Ancillary` for finding extrema with indices.
-}
extremaWithIndicesSuite : Test
extremaWithIndicesSuite =
    describe "Finding Extrema With Indices"
        [ describe "indexedMaximum"
            [ fuzz (fuzzNonempty Fuzz.string) "should return correct index" <|
                \l ->
                    NE.indexedMap Tuple.pair l
                        |> NE.toList
                        |> ListX.maximumBy Tuple.second
                        |> Expect.equal (Just <| indexedMaximum l)
            , test "returns first result" <|
                \() ->
                    Expect.equal ( 1, 5 ) <| indexedMaximum (Nonempty 2 [ 5, 5 ])
            ]
        , describe "indexedMaximumBy"
            [ fuzz (fuzzNonempty fuzzRecord) "should return correct index by a" <|
                \l ->
                    NE.indexedMap Tuple.pair l
                        |> NE.toList
                        |> ListX.maximumBy (.a << Tuple.second)
                        |> Expect.equal (Just <| indexedMaximumBy .a l)
            , fuzz (fuzzNonempty fuzzRecord) "should return correct index by b" <|
                \l ->
                    NE.indexedMap Tuple.pair l
                        |> NE.toList
                        |> ListX.maximumBy (.b << Tuple.second)
                        |> Expect.equal (Just <| indexedMaximumBy .b l)
            , test "returns first result" <|
                \() ->
                    Nonempty { a = 2, b = 5 } [ { a = 4, b = 1 }, { a = 4, b = 2 } ]
                        |> indexedMaximumBy .a
                        |> Expect.equal ( 1, { a = 4, b = 1 } )
            ]
        , describe "indexedMaximumWith"
            [ fuzz (fuzzNonempty fuzzRecord) "should return correct index" <|
                \l ->
                    let
                        f : { a : comparable, b : a } -> { a : comparable, b : a } -> Order
                        f x y =
                            compare x.a y.a
                    in
                    Expect.equal (Just <| indexedMaximumWith f l) (ListX.maximumWith (\( _, x ) ( _, y ) -> f x y) <| NE.toList <| NE.indexedMap Tuple.pair l)
            , test "returns first result" <|
                \() ->
                    let
                        f : { a : comparable, b : a } -> { a : comparable, b : a } -> Order
                        f x y =
                            compare x.a y.a
                    in
                    Expect.equal ( 1, { a = 4, b = 1 } ) <| indexedMaximumWith f (Nonempty { a = 2, b = 5 } [ { a = 4, b = 1 }, { a = 4, b = 2 } ])
            ]
        , describe "indexedMinimum"
            [ fuzz (fuzzNonempty Fuzz.string) "should return correct index" <|
                \l ->
                    NE.indexedMap Tuple.pair l
                        |> NE.toList
                        |> ListX.minimumBy Tuple.second
                        |> Expect.equal (Just <| indexedMinimum l)
            , test "returns first result" <|
                \() ->
                    Expect.equal ( 0, 2 ) <| indexedMinimum (Nonempty 2 [ 5, 2 ])
            ]
        , describe "indexedMinimumBy"
            [ fuzz (fuzzNonempty fuzzRecord) "should return correct index by a" <|
                \l ->
                    NE.indexedMap Tuple.pair l
                        |> NE.toList
                        |> ListX.minimumBy (.a << Tuple.second)
                        |> Expect.equal (Just <| indexedMinimumBy .a l)
            , fuzz (fuzzNonempty fuzzRecord) "should return correct index by b" <|
                \l ->
                    NE.indexedMap Tuple.pair l
                        |> NE.toList
                        |> ListX.minimumBy (.b << Tuple.second)
                        |> Expect.equal (Just <| indexedMinimumBy .b l)
            , test "returns first result" <|
                \() ->
                    Nonempty { a = 2, b = 5 } [ { a = 4, b = 1 }, { a = 2, b = 2 } ]
                        |> indexedMinimumBy .a
                        |> Expect.equal ( 0, { a = 2, b = 5 } )
            ]
        , describe "indexedMinimumWith"
            [ fuzz (fuzzNonempty fuzzRecord) "should return correct index" <|
                \l ->
                    let
                        f : { a : comparable, b : a } -> { a : comparable, b : a } -> Order
                        f x y =
                            compare x.a y.a
                    in
                    NE.indexedMap Tuple.pair l
                        |> NE.toList
                        |> ListX.minimumWith (\( _, x ) ( _, y ) -> f x y)
                        |> Expect.equal (Just <| indexedMinimumWith f l)
            , test "returns first result" <|
                \() ->
                    let
                        f : { a : comparable, b : a } -> { a : comparable, b : a } -> Order
                        f x y =
                            compare x.a y.a
                    in
                    Expect.equal ( 0, { a = 2, b = 5 } ) <| indexedMinimumWith f (Nonempty { a = 2, b = 5 } [ { a = 4, b = 1 }, { a = 2, b = 2 } ])
            ]
        ]


{-| Test suite for `List.Nonempty.Ancillary` for dealing with `Maybe`s.
-}
maybeSuite : Test
maybeSuite =
    describe "`Maybe`s"
        [ describe "combine"
            [ fuzz
                (Fuzz.frequency
                    [ ( 1, Fuzz.constant Nothing )
                    , ( 9, Fuzz.map Just Fuzz.int )
                    ]
                    |> fuzzNonempty
                )
                "If any is `Nothing`, combine outputs `Nothing`"
              <|
                \xs ->
                    combine xs
                        |> (if NE.any ((==) Nothing) xs then
                                Expect.equal Nothing

                            else
                                Expect.equal (Just <| List.filterMap identity <| NE.toList xs) << Maybe.map NE.toList
                           )
            ]
        , describe "traverse"
            [ fuzz
                (Fuzz.frequency
                    [ ( 1, Fuzz.constant [] )
                    , ( 9, Fuzz.map2 (::) Fuzz.int (Fuzz.list Fuzz.int) )
                    ]
                    |> fuzzNonempty
                )
                "If any function output is `Nothing`, combine outputs `Nothing`"
              <|
                \xs ->
                    traverse List.head xs
                        |> (if NE.any List.isEmpty xs then
                                Expect.equal Nothing

                            else
                                Expect.equal (Just <| List.filterMap List.head <| NE.toList xs) << Maybe.map NE.toList
                           )
            ]
        ]


{-| Test suite for `List.Nonempty.Ancillary` for JSON decoding/encoding.
-}
jsonSuite : Test
jsonSuite =
    describe "JSON Decoders/Encoders"
        [ TestX.describeDecoder "decodeArray"
            (decodeArray Decode.int)
            (NE.map String.fromInt
                >> NE.toList
                >> String.join ", "
            )
            [ ( "true", FailsToDecode )
            , ( "42", FailsToDecode )
            , ( "3.14", FailsToDecode )
            , ( "\"hello\"", FailsToDecode )
            , ( "{ \"hello\": 42 }", FailsToDecode )
            , ( "[1]", DecodesTo <| NE.fromElement 1 )
            , ( "[]", FailsToDecode )
            , ( "[1, 2, 3]", DecodesTo <| Nonempty 1 [ 2, 3 ] )
            ]
        , fuzz (fuzzNonempty Fuzz.int) "encodeArray can always be decoded by decodeArray" <|
            \l ->
                Expect.equal (Ok l) <| Decode.decodeValue (decodeArray Decode.int) <| encodeArray Encode.int l
        , TestX.describeDecoder "decodeObject"
            (decodeObject Decode.int)
            (NE.map String.fromInt
                >> NE.toList
                >> String.join ", "
            )
            [ ( "true", FailsToDecode )
            , ( "42", FailsToDecode )
            , ( "3.14", FailsToDecode )
            , ( "\"head\"", FailsToDecode )
            , ( "{ \"head\": 42 }", FailsToDecode )
            , ( "[1]", FailsToDecode )
            , ( "[]", FailsToDecode )
            , ( "[1, 2, 3]", FailsToDecode )
            , ( "{ \"head\": 42, \"tail\": [] }", DecodesTo <| NE.fromElement 42 )
            , ( "{ \"head\": 42, \"tail\": [117, 1] }", DecodesTo <| Nonempty 42 [ 117, 1 ] )
            ]
        , fuzz (fuzzNonempty Fuzz.int) "encodeObject can always be decoded by decodeObject" <|
            \l ->
                Expect.equal (Ok l) <| Decode.decodeValue (decodeObject Decode.int) <| encodeObject Encode.int l
        ]


{-| Test suite for `List.Nonempty.Ancillary` for random functions.
-}
randomSuite : Test
randomSuite =
    describe "Random"
        [ describe "shuffle"
            [ fuzz2 Fuzz.int (fuzzNonempty Fuzz.int) "sorting -> shuffling -> sorting a list should be a no-op" <|
                \seed l ->
                    let
                        sorted : Nonempty Int
                        sorted =
                            NE.sort l
                    in
                    Random.initialSeed seed
                        |> Random.step (shuffle sorted)
                        |> Tuple.first
                        |> NE.sort
                        |> NE.toList
                        |> Expect.equalLists (NE.toList sorted)
            ]
        ]