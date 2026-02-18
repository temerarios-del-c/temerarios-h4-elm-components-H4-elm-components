module TestSuite exposing (..)

import Expect
import Fuzz
import Helper exposing (..)
import Html exposing (..)
import Html.Attributes exposing (href)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


joinWordsTest : Test
joinWordsTest =
    describe "Testing joinWords function"
        [ test "joinWords \"\" \"\" should reduce to \"\"" <|
            \_ ->
                joinWords "" ""
                    |> Expect.equal ""
        , test "joinWords \"a\" \"b\" should reduce to \"ab\"" <|
            \_ ->
                joinWords "a" "b"
                    |> Expect.equal "ab"
        , fuzz Fuzz.string "joinWords str1 str1 should reduce to str1++str1" <|
            \word ->
                joinWords word word
                    |> Expect.equal (word ++ word)
        , fuzz2 Fuzz.string Fuzz.string "joinWords str1 str2 should reduce to str1++str2" <|
            \word word2 ->
                joinWords word word2
                    |> Expect.equal (word ++ word2)
        ]


isUpperCharsTest : Test
isUpperCharsTest =
    describe "Testing isUpperChars function"
        [ test "isUpperChars ['a','a','a'] => [False, False, False]" <|
            \_ ->
                isUpperChars [ 'a', 'a', 'a' ]
                    |> Expect.equal [ False, False, False ]
        , test "isUpperChars ['a','A', 'a', 'A'] => [False, True, False, True]" <|
            \_ ->
                isUpperChars [ 'a', 'A', 'a', 'A' ]
                    |> Expect.equal [ False, True, False, True ]
        , fuzz (Fuzz.list Fuzz.char) "isUpperChars should evaluate all Chars and mark the upper ones " <|
            \list ->
                isUpperChars list
                    |> Expect.equal (List.map Char.isUpper list)
        ]


evalCharsTest : Test
evalCharsTest =
    describe "Testing evalChars function"
        [ test "evalChars ['A','0','a','-','Σ'] Char.isUpper=> [True, False, False, False, False]" <|
            \_ ->
                evalChars [ 'A', '0', 'a', '-', 'Σ' ] Char.isUpper
                    |> Expect.equal [ True, False, False, False, False ]
        , test "evalChars ['A','0','a','-','Σ'] Char.isLower => [False, False, True, False, False]" <|
            \_ ->
                evalChars [ 'A', '0', 'a', '-', 'Σ' ] Char.isLower
                    |> Expect.equal [ False, False, True, False, False ]
        , test "evalChars ['A','0','a','-','Σ'] Char.isAlpha => [True, False, True, False, False]" <|
            \_ ->
                evalChars [ 'A', '0', 'a', '-', 'Σ' ] Char.isAlpha
                    |> Expect.equal [ True, False, True, False, False ]
        , test "evalChars ['A','0','a','-','Σ'] Char.isDigit  => [False, True, False, False, False]" <|
            \_ ->
                evalChars [ 'A', '0', 'a', '-', 'Σ' ] Char.isDigit
                    |> Expect.equal [ False, True, False, False, False ]
        ]


headersTest : Test
headersTest =
    describe "Testing header function"
        [ test "headers \"\" should reduce to \n<div>\n  <h1></h1>\n  <h2></h2>\n  <h3></h3>\n  <h4></h4>\n  <h5></h5>\n  <h6></h6>\n</div>" <|
            \_ ->
                let
                    query : Query.Single msg
                    query =
                        headers ""
                            |> Query.fromHtml
                in
                Expect.all
                    [ \_ -> query |> Query.has [ Selector.tag "div" ]
                    , \_ -> query |> Query.children [] |> Query.count (Expect.equal 6)
                    ]
                    ()
        , test "headers \"a\" should reduce to \n<div>\n  <h1>a</h1>\n  <h2>a</h2>\n  <h3>a</h3>\n  <h4>a</h4>\n  <h5>a</h5>\n  <h6>a</h6>\n</div>" <|
            \_ ->
                let
                    content : String
                    content =
                        "a"

                    query : Query.Single msg
                    query =
                        headers content
                            |> Query.fromHtml

                    children : Query.Multiple msg
                    children =
                        query |> Query.children []
                in
                Expect.all
                    [ \_ -> query |> Query.has [ Selector.tag "div" ]
                    , \_ -> children |> Query.count (Expect.equal 6)
                    , \_ -> children |> Query.index 0 |> Query.has [ Selector.tag "h1", Selector.exactText content ]
                    , \_ -> children |> Query.index 1 |> Query.has [ Selector.tag "h2", Selector.exactText content ]
                    , \_ -> children |> Query.index 2 |> Query.has [ Selector.tag "h3", Selector.exactText content ]
                    , \_ -> children |> Query.index 3 |> Query.has [ Selector.tag "h4", Selector.exactText content ]
                    , \_ -> children |> Query.index 4 |> Query.has [ Selector.tag "h5", Selector.exactText content ]
                    , \_ -> children |> Query.index 5 |> Query.has [ Selector.tag "h6", Selector.exactText content ]
                    ]
                    ()
        , fuzz Fuzz.string "headers {{str}} should reduce to \n<div>\n  <h1>{{str}}</h1>\n  <h2>{{str}}</h2>\n  <h3>{{str}}</h3>\n  <h4>{{str}}</h4>\n  <h5>{{str}}</h5>\n  <h6>{{str}}</h6>\n</div>" <|
            \str ->
                let
                    query : Query.Single msg
                    query =
                        headers str
                            |> Query.fromHtml

                    children : Query.Multiple msg
                    children =
                        query |> Query.children []
                in
                Expect.all
                    [ \_ -> query |> Query.has [ Selector.tag "div" ]
                    , \_ -> children |> Query.count (Expect.equal 6)
                    , \_ -> children |> Query.index 0 |> Query.has [ Selector.tag "h1", Selector.exactText str ]
                    , \_ -> children |> Query.index 1 |> Query.has [ Selector.tag "h2", Selector.exactText str ]
                    , \_ -> children |> Query.index 2 |> Query.has [ Selector.tag "h3", Selector.exactText str ]
                    , \_ -> children |> Query.index 3 |> Query.has [ Selector.tag "h4", Selector.exactText str ]
                    , \_ -> children |> Query.index 4 |> Query.has [ Selector.tag "h5", Selector.exactText str ]
                    , \_ -> children |> Query.index 5 |> Query.has [ Selector.tag "h6", Selector.exactText str ]
                    ]
                    ()
        ]


hyperlinkTest : Test
hyperlinkTest =
    describe "Testing hyperlink function"
        [ test "hyperlink \"\" \"\" should reduce to \n<a href=\"\"></a>" <|
            \_ ->
                hyperlink "" ""
                    |> Query.fromHtml
                    |> Query.has [ Selector.attribute (href "") ]
        , fuzz2 Fuzz.string Fuzz.string "hyperlink randomText1 randomText2 should reduce to \n<a href=\"randomText1\">randomText2</a>" <|
            \str1 str2 ->
                hyperlink str1 str2
                    |> Query.fromHtml
                    |> Query.has [ Selector.attribute (href str1), Selector.exactText str2 ]
        ]
