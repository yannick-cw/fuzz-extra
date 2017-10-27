module ExtraSpec exposing (..)

import Expect
import Fuzz as F
import Fuzz.Extra exposing (oneOf, pick, someOf)
import Test exposing (Test, describe, fuzz, test)


all : Test
all =
    describe "Fuzz-Extra"
        [ describe "oneOf"
            [ fuzz (oneOf [ 1 ]) "A single element list gives the element" <|
                \int -> Expect.equal 1 int
            , test "An empty list fails" <|
                \_ -> Expect.err (oneOf [])
            , fuzz (oneOf [ 1, 2, 3, 4, 5, 6, 7, 8 ]) "The element is always contained in the original list" <|
                \int ->
                    List.member int [ 1, 2, 3, 4, 5, 6, 7, 8 ]
                        |> Expect.true "Expected the element to be in the list"
            ]
        , describe "pick"
            [ fuzz (pick 0 []) "Of an empty list gives the empty list" <|
                \list -> Expect.equal [] list
            , test "Of more elements than present fails" <|
                \_ -> Expect.err (pick 10 [])
            , fuzz (pick 1 [ 1, 2, 3, 4 ]) "1 gives a single element" <|
                \list ->
                    List.length list
                        |> Expect.equal 1
            , fuzz (pick 3 [ 1, 2, 3, 4 ]) "The elements are always contained in the original list" <|
                \list ->
                    list
                        |> List.all (\a -> List.member a [ 1, 2, 3, 4 ])
                        |> Expect.true "Expected all elements to be in the list"
            , fuzz (pick 4 [ 2, 4, 1, 3 ]) "The sorted picked list must be the original sorted list" <|
                \list ->
                    list
                        |> List.sort
                        |> Expect.equal [ 1, 2, 3, 4 ]
            ]
        , describe "someOf"
            [ fuzz (someOf []) "Of an empty list gives the empty list" <|
                \list -> Expect.equal [] list
            , fuzz (someOf [ 1, 2, 3, 4 ]) "The elements are always contained in the original list" <|
                \list ->
                    list
                        |> List.all (\a -> List.member a [ 1, 2, 3, 4 ])
                        |> Expect.true "Expected all elements to be in the list"
            ]
        ]
