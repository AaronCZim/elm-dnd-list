module AllTests exposing (..)

import Test exposing (describe, test)
import Expect
import DnDList exposing (..)


all =
    describe "All DnDList Tests"
        [ test """Append "hello" to empty""" <|
            \() ->
                (init |> update (Append "hello"))
                    |> Expect.equal (fromList [ "hello" ])
        , test """Append "hi" to ["hello"]""" <|
            \() ->
                (fromList [ "hello" ] |> update (Append "hi"))
                    |> Expect.equal (fromList [ "hello", "hi" ])
        , test """Remove 0 from ["hello", "hi"]""" <|
            \() ->
                (fromList [ "hello", "hi" ]
                    |> update (Remove (Just (SingleSelection 0)))
                )
                    |> Expect.equal (fromList [ "hi" ])
        , test """Remove 1 from ["hello", "hi"]""" <|
            \() ->
                (fromList [ "hello", "hi" ]
                    |> update (Remove (Just (SingleSelection 1)))
                )
                    |> Expect.equal (fromList [ "hello" ])
        , test "select 0 from NoSelection" <|
            \() ->
                (init3 |> update (Click 0) |> .selection)
                    |> Expect.equal
                        (Just (SingleSelection 0))
        , test "select 1 from 0" <|
            \() ->
                (init3 |> update (Click 1) |> .selection)
                    |> Expect.equal
                        (Just (SingleSelection 1))
        , test "inSelection: Yes (Single Selection)" <|
            \() ->
                (inSelection 0 (Just (SingleSelection 0)))
                    |> Expect.equal
                        True
        , test "inSelection: No (No Selection)" <|
            \() ->
                (inSelection 0 Nothing)
                    |> Expect.equal False
        , test "inSelection: No (Single Selection, Miss)" <|
            \() ->
                (inSelection 1 (Just (SingleSelection 0)))
                    |> Expect.equal False
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
        ]


init3List =
    [ "one", "two", "three" ]


init3 =
    fromList init3List
