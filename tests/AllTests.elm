module AllTests exposing (..)

import Test exposing (describe, test)
import Expect
import DnDList exposing (..)


all =
    describe "All DnDList Tests"
        [ test """Append "hello" to empty""" <|
            \() ->
                (init |> updateModel (Append "hello"))
                    |> Expect.equal (fromList [ "hello" ])
        , test """Append "hi" to ["hello"]""" <|
            \() ->
                (fromList [ "hello" ] |> updateModel (Append "hi"))
                    |> Expect.equal (fromList [ "hello", "hi" ])
        , test """Remove 0 from ["hello", "hi"]""" <|
            \() ->
                (fromList [ "hello", "hi" ]
                    |> updateModel (Remove 0)
                )
                    |> Expect.equal (fromList [ "hi" ])
        , test """Remove 1 from ["hello", "hi"]""" <|
            \() ->
                (fromList [ "hello", "hi" ]
                    |> updateModel (Remove 1)
                )
                    |> Expect.equal (fromList [ "hello" ])
        , test """swap 0 -1 ["one", "two", "three"]""" <|
            \() ->
                (init3List |> swap 0 -1)
                    |> Expect.equal
                        Nothing
        , test """swap 1 3 ["one", "two", "three"]""" <|
            \() ->
                (init3List |> swap 0 -1)
                    |> Expect.equal
                        Nothing
        , test """swap 0 0 ["one", "two", "three"]""" <|
            \() ->
                (init3List |> swap 0 0)
                    |> Expect.equal
                        (Just init3List)
        , test """swap 0 1 ["one", "two", "three"]""" <|
            \() ->
                (init3List |> swap 0 1)
                    |> Expect.equal
                        (Just [ "two", "one", "three" ])
        , test """swap 0 2 ["one", "two", "three"]""" <|
            \() ->
                (init3List |> swap 0 2)
                    |> Expect.equal
                        (Just [ "three", "two", "one" ])
        , test """swap 2 1 ["one", "two", "three"]""" <|
            \() ->
                (init3List |> swap 2 1)
                    |> Expect.equal
                        (Just [ "one", "three", "two" ])
        , test """swap 2 1 ["zero", "one", "two", "three"]""" <|
            \() ->
                (("zero" :: init3List) |> swap 2 1)
                    |> Expect.equal
                        (Just [ "zero", "two", "one", "three" ])
        , test """set 0 to zero in init3List""" <|
            \() ->
                (init3List |> set 0 "zero")
                    |> Expect.equal
                        [ "zero", "two", "three" ]
        ]


init3List =
    [ "one", "two", "three" ]


init3 =
    fromList init3List
