module Stylus exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import List exposing (foldr)
import Parser.Advanced exposing (Step(..), run)
import String exposing (join)
import Stylus.Parser exposing (..)
import Test exposing (..)
import Tuple exposing (..)


makeRule : ( List String, List ( String, String ) ) -> Expression
makeRule ( selectors, declarations ) =
    Rule ( selectors, { declarations = declarations, nestedRules = [] } )


newlineFold : List String -> String
newlineFold =
    foldr (\a b -> a ++ "\n" ++ b) ""


primitiveTests : Test
primitiveTests =
    describe "Primitives"
        [ test "Selector" <|
            \_ ->
                Expect.equal (Ok "main[foo=bar]")
                    (run selector "main[foo=bar]")
        , test "Selectors" <|
            \_ ->
                Expect.equal
                    (Ok [ "h1", "h2", "h3 p", ".error", "main[foo=bar]" ])
                    (run selectors "h1, h2, h3 p, .error, main[foo=bar]\n")
        , test "Declaration" <|
            \_ ->
                Expect.equal
                    (Ok (Loop [ ( "display", "inline-block" ) ]))
                    (run (declaration []) "  display inline-block\n")
        , test "Declarations" <|
            \_ ->
                let
                    declarationsStr =
                        "  display inline-block\n"
                            ++ "  margin 0\n"
                            ++ "  font-size 12\n"
                in
                Expect.equal
                    (Ok
                        [ ( "display", "inline-block" )
                        , ( "margin", "0" )
                        , ( "font-size", "12" )
                        ]
                    )
                    (run declarations declarationsStr)
        , test "Rule with rule" <|
            \_ ->
                Expect.equal
                    (Ok
                        (makeRule
                            ( [ "h1.important" ]
                            , [ ( "display", "inline-block" )
                              , ( "margin", "0" )
                              , ( "font-size", "2em" )
                              ]
                            )
                        )
                    )
                    (run rule
                        ("h1.important\n"
                            ++ "  display inline-block\n"
                            ++ "  margin 0\n"
                            ++ "  font-size 2em\n"
                        )
                    )
        , test "Comment" <|
            \_ ->
                Expect.equal (Ok (Comment "Just a test"))
                    (run commentLine "// Just a test\n")
        , test "Newlines" <|
            \_ ->
                Expect.equal (Ok Newlines) (run newlines "\n\n\n")
        , test "No Newlines" <|
            \_ ->
                Expect.equal
                    (Err
                        [ { col = 1
                          , contextStack = []
                          , problem = GenericProblem
                          , row = 1
                          }
                        ]
                    )
                    (run newlines "")
        ]


stylusTests : Test
stylusTests =
    describe "Stylus Parsing"
        [ test "Rule" <|
            \_ ->
                Expect.equal
                    (Ok
                        (makeRule
                            ( [ "h1.important" ]
                            , [ ( "display", "inline-block" )
                              , ( "margin", "0" )
                              , ( "font-size", "2em" )
                              ]
                            )
                        )
                    )
                    (run rule
                        ("h1.important\n"
                            ++ "  display inline-block\n"
                            ++ "  margin 0\n"
                            ++ "  font-size 2em\n"
                        )
                    )
        , test "Section" <|
            \_ ->
                Expect.equal
                    (Ok
                        (Loop
                            [ makeRule
                                ( [ "h1.important" ]
                                , [ ( "display", "inline-block" )
                                  , ( "margin", "0" )
                                  , ( "font-size", "2em" )
                                  ]
                                )
                            ]
                        )
                    )
                    (run (section [])
                        ("h1.important\n"
                            ++ "  display inline-block\n"
                            ++ "  margin 0\n"
                            ++ "  font-size 2em\n"
                        )
                    )
        , test "Rules" <|
            \_ ->
                Expect.equal
                    (Ok
                        [ makeRule
                            ( [ "h1.important" ]
                            , [ ( "display"
                                , "inline-block"
                                )
                              , ( "margin", "0" )
                              , ( "font-size", "2em" )
                              ]
                            )
                        , makeRule
                            ( [ ".alert" ]
                            , [ ( "color"
                                , "rgb(50, 50, 50)"
                                )
                              ]
                            )
                        , makeRule
                            ( [ ".primary" ]
                            , [ ( "font-weight"
                                , "900"
                                )
                              ]
                            )
                        ]
                    )
                    (run stylus
                        (""
                            ++ "h1.important\n"
                            ++ "  display inline-block\n"
                            ++ "  margin 0\n"
                            ++ "  font-size 2em\n"
                            ++ ".alert\n"
                            ++ "  color rgb(50, 50, 50)\n"
                            ++ ".primary\n"
                            ++ "  font-weight 900\n"
                        )
                    )
        , test "Rules With Comments" <|
            \_ ->
                Expect.equal
                    (Ok
                        [ makeRule
                            ( [ "h1.important" ]
                            , [ ( "display", "inline-block" )
                              , ( "margin", "0" )
                              , ( "font-size", "2em" )
                              ]
                            )
                        , Comment "Just like that"
                        , makeRule
                            ( [ ".alert" ]
                            , [ ( "color"
                                , "rgb(50, 50, 50)"
                                )
                              ]
                            )
                        , Comment "Even more"
                        , Comment "Wow, so much comment"
                        , Comment "A lot of special chars : . // $ % ^ *"
                        ]
                    )
                    (run stylus
                        (""
                            ++ "h1.important\n"
                            ++ "  display inline-block\n"
                            ++ "  margin 0\n"
                            ++ "  font-size 2em\n"
                            ++ "// Just like that\n"
                            ++ ".alert\n"
                            ++ "  color rgb(50, 50, 50)\n"
                            ++ "// Even more\n"
                            ++ "// Wow, so much comment\n"
                            ++ "// A lot of special chars : . // $ % ^ *\n"
                        )
                    )
        , test "Rules With Newlines" <|
            \_ ->
                Expect.equal
                    (Ok
                        [ makeRule
                            ( [ "h1.important" ]
                            , [ ( "display"
                                , "inline-block"
                                )
                              , ( "margin", "0" )
                              , ( "font-size", "2em" )
                              ]
                            )
                        , Newlines
                        , Comment "Just like that"
                        , Newlines
                        , makeRule
                            ( [ ".alert" ]
                            , [ ( "color"
                                , "rgb(50, 50, 50)"
                                )
                              ]
                            )
                        ]
                    )
                    (run stylus
                        (""
                            ++ "h1.important\n"
                            ++ "  display inline-block\n"
                            ++ "  margin 0\n"
                            ++ "  font-size 2em\n"
                            ++ "\n"
                            ++ "// Just like that\n"
                            ++ "\n"
                            ++ "\n"
                            ++ ".alert\n"
                            ++ "  color rgb(50, 50, 50)\n"
                        )
                    )
        ]


stylusAstToCssTests : Test
stylusAstToCssTests =
    describe "Stylus AST to CSS"
        [ test "Rule" <|
            \_ ->
                Expect.equal
                    ("h1.important"
                        ++ "{display:inline-block;margin:0;font-size:2em}\n"
                    )
                    (serializeStylusAst
                        [ makeRule
                            ( [ "h1.important" ]
                            , [ ( "display", "inline-block" )
                              , ( "margin", "0" )
                              , ( "font-size", "2em" )
                              ]
                            )
                        ]
                    )
        , test "Rules" <|
            \_ ->
                Expect.equal
                    ("h1.important"
                        ++ "{display:inline-block;margin:0;font-size:2em}\n"
                        ++ ".alert{color:rgb(50, 50, 50)}\n"
                        ++ ".primary{font-weight:900}\n"
                    )
                    (serializeStylusAst
                        [ makeRule
                            ( [ "h1.important" ]
                            , [ ( "display"
                                , "inline-block"
                                )
                              , ( "margin", "0" )
                              , ( "font-size", "2em" )
                              ]
                            )
                        , makeRule
                            ( [ ".alert" ]
                            , [ ( "color"
                                , "rgb(50, 50, 50)"
                                )
                              ]
                            )
                        , makeRule
                            ( [ ".primary" ]
                            , [ ( "font-weight"
                                , "900"
                                )
                              ]
                            )
                        ]
                    )
        , test "Rule With Comments" <|
            \_ ->
                Expect.equal
                    (""
                        ++ "h1.important"
                        ++ "{display:inline-block;margin:0;font-size:2em}\n"
                        ++ "/*Just like that*/\n"
                        ++ ".alert{color:rgb(50, 50, 50)}\n"
                        ++ "/*Even more*/\n"
                        ++ "/*Wow, so much comment*/\n"
                        ++ "/*A lot of special chars : . // $ % ^ **/\n"
                    )
                    (serializeStylusAst
                        [ makeRule
                            ( [ "h1.important" ]
                            , [ ( "display", "inline-block" )
                              , ( "margin", "0" )
                              , ( "font-size", "2em" )
                              ]
                            )
                        , Comment "Just like that"
                        , makeRule
                            ( [ ".alert" ]
                            , [ ( "color"
                                , "rgb(50, 50, 50)"
                                )
                              ]
                            )
                        , Comment "Even more"
                        , Comment "Wow, so much comment"
                        , Comment "A lot of special chars : . // $ % ^ *"
                        ]
                    )
        , test "Rules With Newlines" <|
            \_ ->
                Expect.equal
                    (""
                        ++ "\n"
                        ++ "h1.important"
                        ++ "{display:inline-block;margin:0;font-size:2em}\n"
                        ++ "\n"
                        ++ "/*Just like that*/\n"
                        ++ "\n"
                        ++ ".alert{color:rgb(50, 50, 50)}\n"
                        ++ "\n"
                    )
                    (serializeStylusAst
                        [ Newlines
                        , makeRule
                            ( [ "h1.important" ]
                            , [ ( "display"
                                , "inline-block"
                                )
                              , ( "margin", "0" )
                              , ( "font-size", "2em" )
                              ]
                            )
                        , Newlines
                        , Comment "Just like that"
                        , Newlines
                        , makeRule
                            ( [ ".alert" ]
                            , [ ( "color"
                                , "rgb(50, 50, 50)"
                                )
                              ]
                            )
                        , Newlines
                        ]
                    )
        ]


stylusToCssTests : Test
stylusToCssTests =
    describe "Stylus to CSS"
        [ test "Rule" <|
            \_ ->
                Expect.equal
                    (Ok
                        ("h1.important"
                            ++ "{display:inline-block;margin:0;font-size:2em}\n"
                        )
                    )
                    (stylusToCss
                        (""
                            ++ "h1.important\n"
                            ++ "  display inline-block\n"
                            ++ "  margin 0\n"
                            ++ "  font-size 2em\n"
                        )
                    )
        , test "Rules" <|
            \_ ->
                Expect.equal
                    (Ok
                        (""
                            ++ "h1.important"
                            ++ "{display:inline-block;margin:0;font-size:2em}\n"
                            ++ ".alert{color:rgb(50, 50, 50)}\n"
                            ++ ".primary{font-weight:900}\n"
                        )
                    )
                    (stylusToCss
                        (""
                            ++ "h1.important\n"
                            ++ "  display inline-block\n"
                            ++ "  margin 0\n"
                            ++ "  font-size 2em\n"
                            ++ ".alert\n"
                            ++ "  color rgb(50, 50, 50)\n"
                            ++ ".primary\n"
                            ++ "  font-weight 900\n"
                        )
                    )
        , test "Rule With Comments" <|
            \_ ->
                Expect.equal
                    (Ok
                        (""
                            ++ "h1.important"
                            ++ "{display:inline-block;margin:0;font-size:2em}\n"
                            ++ "/*Just like that*/\n"
                            ++ ".alert{color:rgb(50, 50, 50)}\n"
                            ++ "/*Even more*/\n"
                            ++ "/*Wow, so much comment*/\n"
                            ++ "/*A lot of special chars : . // $ % ^ **/\n"
                        )
                    )
                    (stylusToCss
                        (""
                            ++ "h1.important\n"
                            ++ "  display inline-block\n"
                            ++ "  margin 0\n"
                            ++ "  font-size 2em\n"
                            ++ "// Just like that\n"
                            ++ ".alert\n"
                            ++ "  color rgb(50, 50, 50)\n"
                            ++ "// Even more\n"
                            ++ "// Wow, so much comment\n"
                            ++ "// A lot of special chars : . // $ % ^ *\n"
                        )
                    )
        , test "Rules With Newlines" <|
            \_ ->
                Expect.equal
                    (Ok
                        (""
                            ++ "h1.important"
                            ++ "{display:inline-block;margin:0;font-size:2em}\n"
                            ++ "/*Just like that*/\n"
                            ++ ".alert{color:rgb(50, 50, 50)}\n"
                        )
                    )
                    (stylusToCss
                        (""
                            ++ "h1.important\n"
                            ++ "  display inline-block\n"
                            ++ "  margin 0\n"
                            ++ "  font-size 2em\n"
                            ++ "// Just like that\n"
                            ++ ".alert\n"
                            ++ "  color rgb(50, 50, 50)\n"
                        )
                    )
        ]


nestedRulesTests : Test
nestedRulesTests =
    describe "Nested Rules"
        [ test "Simple nested rule" <|
            \_ ->
                let
                    stylusStr =
                        ""
                            ++ ".navbar\n"
                            ++ "  background blue\n"
                            ++ "  ul\n"
                            ++ "    list-style none\n"

                    expectedCss =
                        ""
                            ++ ".navbar{background:blue}\n"
                            ++ ".navbar ul{list-style:none}\n"
                in
                Expect.equal (Ok expectedCss) (stylusToCss stylusStr)
        , test "Multiple levels of nesting" <|
            \_ ->
                let
                    stylusStr =
                        ""
                            ++ ".navbar\n"
                            ++ "  background blue\n"
                            ++ "  ul\n"
                            ++ "    list-style none\n"
                            ++ "    li\n"
                            ++ "      padding 5px\n"

                    expectedCss =
                        ""
                            ++ ".navbar{background:blue}\n"
                            ++ ".navbar ul{list-style:none}\n"
                            ++ ".navbar ul li{padding:5px}\n"
                in
                Expect.equal (Ok expectedCss) (stylusToCss stylusStr)
        , test "Multiple nested rules with mixed content" <|
            \_ ->
                let
                    stylusStr =
                        ""
                            ++ ".container\n"
                            ++ "  width 100%\n"
                            ++ "  .header\n"
                            ++ "    background red\n"
                            ++ "  .footer\n"
                            ++ "    background blue\n"
                            ++ "    color white\n"

                    expectedCss =
                        ""
                            ++ ".container{width:100%}\n"
                            ++ ".container .header{background:red}\n"
                            ++ ".container .footer{background:blue;color:white}\n"
                in
                Expect.equal (Ok expectedCss) (stylusToCss stylusStr)
        ]


suite : Test
suite =
    describe "Stylus Parser"
        [ describe "Selectors Parser"
            [ test "parsing" <|
                \_ ->
                    Expect.equal (Ok [ "h1", "h2", "h3" ])
                        (run selectors "h1, h2, h3\n")
            ]
        , primitiveTests
        , nestedRulesTests
        , edgeCaseTests
        , errorHandlingTests
        , performanceTests
        , fuzzTests
        , describe "Complete Parser"
            [ test "Parsing of complete file" <|
                \_ ->
                    let
                        stylusStr =
                            ""
                                ++ "h1, h2, h3\n"
                                ++ "  color blue\n"
                                ++ "  font-size 18\n"
                                ++ "// Just a comment\n"
                                ++ "div, section\n"
                                ++ "  margin-bottom 2em\n"
                                ++ "  border 1px solid black\n"

                        cssStr =
                            newlineFold
                                [ "h1, h2, h3{"
                                    ++ "color:blue;"
                                    ++ "font-size:18"
                                    ++ "}"
                                , "/*Just a comment*/"
                                , "div, section{"
                                    ++ "margin-bottom:2em;"
                                    ++ "border:1px solid black"
                                    ++ "}"
                                ]

                        --|> map (\c -> c ++ "\n")
                        --|> join ""
                    in
                    Expect.equal (Ok cssStr) (stylusToCss stylusStr)
            ]
        ]


edgeCaseTests : Test
edgeCaseTests =
    describe "Edge Cases"
        [ test "Rule without declarations" <|
            \_ ->
                Expect.equal
                    (Ok (Rule ( [ ".empty" ], { declarations = [], nestedRules = [] } )))
                    (run rule ".empty\n")
        , test "Value with special characters" <|
            \_ ->
                let
                    specialValue =
                        "rgb(255, 128, 0)"

                    input =
                        "  color " ++ specialValue ++ "\n"
                in
                Expect.equal
                    (Ok (Loop [ ( "color", specialValue ) ]))
                    (run (declaration []) input)
        , test "Nested rule at maximum depth" <|
            \_ ->
                let
                    stylusStr =
                        ".a\n"
                            ++ "  color red\n"
                            ++ "  .b\n"
                            ++ "    color green\n"
                            ++ "    .c\n"
                            ++ "      color blue\n"

                    expectedCss =
                        ".a{color:red}\n"
                            ++ ".a .b{color:green}\n"
                            ++ ".a .b .c{color:blue}\n"
                in
                Expect.equal (Ok expectedCss) (stylusToCss stylusStr)
        , test "Multiple selectors with nested rules" <|
            \_ ->
                let
                    stylusStr =
                        "h1, h2, h3\n"
                            ++ "  margin 0\n"
                            ++ "  span\n"
                            ++ "    font-weight bold\n"

                    expectedCss =
                        "h1, h2, h3{margin:0}\n"
                            ++ "h1 span, h2 span, h3 span{font-weight:bold}\n"
                in
                Expect.equal (Ok expectedCss) (stylusToCss stylusStr)
        , test "Empty input" <|
            \_ ->
                Expect.equal (Ok "") (stylusToCss "")
        , test "Only comments" <|
            \_ ->
                let
                    stylusStr =
                        "// Just a comment\n// Another comment\n"

                    expectedCss =
                        "/*Just a comment*/\n/*Another comment*/\n"
                in
                Expect.equal (Ok expectedCss) (stylusToCss stylusStr)
        , test "Only newlines" <|
            \_ ->
                let
                    stylusStr =
                        "\n\n\n"

                    expectedCss =
                        "\n"
                in
                Expect.equal (Ok expectedCss) (stylusToCss stylusStr)
        ]


errorHandlingTests : Test
errorHandlingTests =
    describe "Error Handling"
        [ test "Empty selector fails" <|
            \_ ->
                Expect.err (run selector "")
        , test "Empty property fails" <|
            \_ ->
                Expect.err (run (declaration []) "  \n")
        ]


performanceTests : Test
performanceTests =
    describe "Performance Tests"
        [ test "Large number of rules" <|
            \_ ->
                let
                    generateRule n =
                        ".rule" ++ String.fromInt n ++ "\n  color red\n"

                    stylusStr =
                        List.range 1 50
                            |> List.map generateRule
                            |> String.concat
                in
                case stylusToCss stylusStr of
                    Ok css ->
                        Expect.equal True (String.contains ".rule50{" css)

                    Err _ ->
                        Expect.fail "Should parse successfully"
        , test "Three-level nesting works" <|
            \_ ->
                let
                    stylusStr =
                        ".level1\n"
                            ++ "  color red\n"
                            ++ "  .level2\n"
                            ++ "    color green\n"
                            ++ "    .level3\n"
                            ++ "      color blue\n"
                in
                case stylusToCss stylusStr of
                    Ok css ->
                        Expect.equal True
                            (String.contains ".level1 .level2 .level3{" css)

                    Err _ ->
                        Expect.fail "Should parse three-level nested rules"
        ]



-- Fuzz test generators


validSelectorChar : Fuzzer Char
validSelectorChar =
    Fuzz.oneOf
        [ Fuzz.char
            |> Fuzz.map
                (\c ->
                    if Char.isAlpha c then
                        c

                    else
                        'a'
                )
        , Fuzz.constant '.'
        , Fuzz.constant '#'
        , Fuzz.constant '-'
        , Fuzz.constant '_'
        ]


validPropertyChar : Fuzzer Char
validPropertyChar =
    Fuzz.oneOf
        [ Fuzz.char
            |> Fuzz.map
                (\c ->
                    if Char.isAlpha c then
                        c

                    else
                        'a'
                )
        , Fuzz.constant '-'
        , Fuzz.constant '_'
        ]


validValueChar : Fuzzer Char
validValueChar =
    Fuzz.oneOf
        [ Fuzz.char
            |> Fuzz.map
                (\c ->
                    if Char.isAlphaNum c then
                        c

                    else
                        'a'
                )
        , Fuzz.constant ' '
        , Fuzz.constant '.'
        , Fuzz.constant ','
        , Fuzz.constant '('
        , Fuzz.constant ')'
        , Fuzz.constant '#'
        , Fuzz.constant '%'
        ]


simpleSelector : Fuzzer String
simpleSelector =
    Fuzz.map2 String.cons
        (Fuzz.oneOf [ Fuzz.constant '.', Fuzz.constant '#', validPropertyChar ])
        (Fuzz.map (String.fromList >> String.left 20) (Fuzz.list validSelectorChar))
        |> Fuzz.map (String.filter (\c -> c /= ' '))



-- Remove spaces which aren't valid at start


simpleProperty : Fuzzer String
simpleProperty =
    Fuzz.map2 String.cons
        validPropertyChar
        (Fuzz.map (String.fromList >> String.left 15) (Fuzz.list validPropertyChar))


simpleValue : Fuzzer String
simpleValue =
    Fuzz.map2 String.cons
        validValueChar
        (Fuzz.map (String.fromList >> String.left 20) (Fuzz.list validValueChar))
        |> Fuzz.map String.trim
        |> Fuzz.map
            (\s ->
                if String.isEmpty s then
                    "default"

                else
                    s
            )


fuzzTests : Test
fuzzTests =
    describe "Additional Robustness Tests"
        [ test "Multiple declarations work" <|
            \_ ->
                let
                    stylusStr =
                        ""
                            ++ ".test\n"
                            ++ "  margin 0\n"
                            ++ "  padding 1\n"
                            ++ "  border 2\n"
                in
                case stylusToCss stylusStr of
                    Ok css ->
                        if String.contains ".test{" css then
                            Expect.pass

                        else
                            Expect.fail ("Expected .test{ in CSS output: " ++ css)

                    Err err ->
                        Expect.fail "Should parse multiple declarations, got error"
        , test "Two-level nesting works" <|
            \_ ->
                let
                    stylusStr =
                        ".level1\n"
                            ++ "  color red\n"
                            ++ "  .level2\n"
                            ++ "    color blue\n"
                in
                case stylusToCss stylusStr of
                    Ok css ->
                        Expect.all
                            [ \_ -> Expect.equal True (String.contains ".level1{color:red}" css)
                            , \_ -> Expect.equal True (String.contains ".level1 .level2{color:blue}" css)
                            ]
                            ()

                    Err _ ->
                        Expect.fail "Should parse two-level nested rules"
        ]
