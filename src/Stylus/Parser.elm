module Stylus.Parser exposing
    ( stylusToCss
    , Expression(..)
    , Problem(..)
    , selector
    , selectors
    , section
    , rule
    , declaration
    , declarations
    , newlines
    , commentLine
    , stylus
    , serializeStylusAst
    )

{-| Convert a strict subset of Stylus to CSS

@docs stylusToCss


## Internal

@docs Expression
@docs Problem
@docs selector
@docs selectors
@docs section
@docs rule
@docs declaration
@docs declarations
@docs newlines
@docs commentLine
@docs stylus
@docs serializeStylusAst

-}

import Char
import Parser.Advanced exposing (..)


type alias StyParser a =
    Parser.Advanced.Parser Context Problem a


type Context
    = Definition String
    | List
    | Record


{-| -}
type Problem
    = BadIndent
    | BadKeyword String
    | GenericProblem


type alias Selector =
    String


type alias Selectors =
    List String


type alias Property =
    String


type alias Value =
    String


type alias Declaration =
    ( Property, Value )


type alias RuleContent =
    { declarations : List Declaration
    , nestedRules : List Expression
    }


{-| -}
type Expression
    = Rule ( Selectors, RuleContent )
    | Comment String
    | Newlines


isNotNewline : Char -> Bool
isNotNewline char =
    char /= '\n'


isValidChar : Char -> Bool
isValidChar char =
    Char.isLower char
        || Char.isUpper char
        || List.member char [ '-', '_' ]


isValidValue : Char -> Bool
isValidValue char =
    isValidChar char
        || Char.isDigit char
        || List.member char [ ' ', '.', ',', '(', ')', '"', '#', '%', '/' ]


isValidSelector : Char -> Bool
isValidSelector char =
    isValidChar char
        || Char.isDigit char
        || List.member char [ ' ', '.', ':', '=', '(', ')', '[', ']' ]


{-| -}
selector : StyParser String
selector =
    getChompedString <|
        succeed ()
            |. chompIf isValidSelector GenericProblem
            |. chompWhile isValidSelector


{-| Parse selectors ending with newline
-}
selectors : StyParser Selectors
selectors =
    sequence
        { start = Token "" GenericProblem
        , separator = Token ", " GenericProblem
        , end = Token "\n" GenericProblem
        , spaces = symbol (Token "" GenericProblem)
        , item = selector
        , trailing = Forbidden
        }


{-| Parse selectors without requiring newline ending (for nested rules)
-}
selectorsNoNewline : StyParser Selectors
selectorsNoNewline =
    succeed (\s -> [ s ])
        |= selector
        |. symbol (Token "\n" GenericProblem)


property : StyParser Property
property =
    getChompedString <|
        succeed ()
            |. chompIf isValidChar GenericProblem
            |. chompWhile isValidChar


value : StyParser Value
value =
    getChompedString <|
        succeed ()
            |. chompIf isValidValue GenericProblem
            |. chompWhile isValidValue


comment : StyParser Expression
comment =
    map Comment (getChompedString (chompUntilEndOr "\n"))


{-| Parse a single declaration or nested rule within a rule block
-}
ruleContentItem :
    RuleContent
    -> StyParser (Step RuleContent RuleContent)
ruleContentItem content =
    oneOf
        [ backtrackable <|
            succeed (\a b -> Loop { content | declarations = ( a, b ) :: content.declarations })
                |. symbol (Token "  " GenericProblem)
                |= property
                |. symbol (Token " " GenericProblem)
                |= value
                |. symbol (Token "\n" GenericProblem)
        , succeed (\nestedRuleExpr -> Loop { content | nestedRules = nestedRuleExpr :: content.nestedRules })
            |. symbol (Token "  " GenericProblem)
            |= nestedRuleIndented
        , succeed ()
            |> map (\_ -> Done { declarations = List.reverse content.declarations, nestedRules = List.reverse content.nestedRules })
        ]


{-| Parse nested rule with proper indentation
-}
nestedRuleIndented : StyParser Expression
nestedRuleIndented =
    inContext (Definition "nested-rule") <|
        succeed (\a b -> Rule ( a, b ))
            |= selectorsNoNewline
            |= nestedRuleContent


{-| Parse content of nested rules with deeper indentation
-}
nestedRuleContent : StyParser RuleContent
nestedRuleContent =
    loop { declarations = [], nestedRules = [] } nestedRuleContentItem


{-| Parse items within nested rules (declarations or further nested rules)
-}
nestedRuleContentItem :
    RuleContent
    -> StyParser (Step RuleContent RuleContent)
nestedRuleContentItem content =
    oneOf
        [ backtrackable <|
            succeed (\a b -> Loop { content | declarations = ( a, b ) :: content.declarations })
                |. symbol (Token "    " GenericProblem)
                -- 4 spaces for nested rule declarations
                |= property
                |. symbol (Token " " GenericProblem)
                |= value
                |. symbol (Token "\n" GenericProblem)
        , succeed (\nestedRuleExpr -> Loop { content | nestedRules = nestedRuleExpr :: content.nestedRules })
            |. symbol (Token "    " GenericProblem)
            -- 4 spaces for further nested rules
            |= furtherNestedRule
        , succeed ()
            |> map (\_ -> Done { declarations = List.reverse content.declarations, nestedRules = List.reverse content.nestedRules })
        ]


{-| Parse further nested rules
-}
furtherNestedRule : StyParser Expression
furtherNestedRule =
    inContext (Definition "further-nested-rule") <|
        succeed (\a b -> Rule ( a, b ))
            |= selectorsNoNewline
            |= furtherNestedRuleContent


{-| Parse content of further nested rules
-}
furtherNestedRuleContent : StyParser RuleContent
furtherNestedRuleContent =
    loop { declarations = [], nestedRules = [] } furtherNestedRuleContentItem


{-| Parse items within further nested rules
-}
furtherNestedRuleContentItem :
    RuleContent
    -> StyParser (Step RuleContent RuleContent)
furtherNestedRuleContentItem content =
    oneOf
        [ backtrackable <|
            succeed (\a b -> Loop { content | declarations = ( a, b ) :: content.declarations })
                |. symbol (Token "      " GenericProblem)
                -- 6 spaces for further nested declarations
                |= property
                |. symbol (Token " " GenericProblem)
                |= value
                |. symbol (Token "\n" GenericProblem)
        , succeed ()
            |> map (\_ -> Done { declarations = List.reverse content.declarations, nestedRules = List.reverse content.nestedRules })
        ]


{-| Parse nested rule (selector + content)
-}
nestedRule : StyParser Expression
nestedRule =
    inContext (Definition "nested-rule") <|
        succeed (\a b -> Rule ( a, b ))
            |= selectors
            |= ruleContent


{-| Parse rule content (declarations and nested rules)
-}
ruleContent : StyParser RuleContent
ruleContent =
    loop { declarations = [], nestedRules = [] } ruleContentItem


{-| Parse a declaration (kept for backwards compatibility)
-}
declaration :
    List Declaration
    -> StyParser (Step (List Declaration) (List Declaration))
declaration dcls =
    oneOf
        [ succeed (\a b -> Loop (( a, b ) :: dcls))
            |. symbol (Token "  " GenericProblem)
            |= property
            |. symbol (Token " " GenericProblem)
            |= value
            |. symbol (Token "\n" GenericProblem)
        , succeed ()
            |> map (\_ -> Done (List.reverse dcls))
        ]


{-| Parse declarations (kept for backwards compatibility)
-}
declarations : StyParser (List Declaration)
declarations =
    loop [] declaration


{-| Parse a top-level rule
-}
rule : StyParser Expression
rule =
    inContext (Definition "rule") <|
        succeed (\a b -> Rule ( a, b ))
            |= selectors
            |= ruleContent


{-| -}
commentLine : StyParser Expression
commentLine =
    inContext (Definition "comment") <|
        succeed identity
            |. symbol (Token "// " GenericProblem)
            |= comment
            |. symbol (Token "\n" GenericProblem)


{-| -}
newlines : StyParser Expression
newlines =
    succeed Newlines
        |. chompIf (\c -> c == '\n') GenericProblem
        |. chompWhile (\c -> c == '\n')


{-| -}
section :
    List Expression
    -> StyParser (Step (List Expression) (List Expression))
section scts =
    oneOf
        [ succeed
            (\start sct end ->
                if start == end then
                    Done (List.reverse scts)

                else
                    Loop (sct :: scts)
            )
            |= getOffset
            |= oneOf
                [ newlines
                , rule
                , commentLine
                ]
            |= getOffset
        , succeed ()
            |> map (\_ -> Done (List.reverse scts))
        ]


{-| -}
stylus : StyParser (List Expression)
stylus =
    loop [] section


flattenRule : List String -> RuleContent -> List Expression
flattenRule parentSelectors content =
    let
        currentRule =
            if List.isEmpty content.declarations then
                []

            else
                [ Rule ( parentSelectors, { declarations = content.declarations, nestedRules = [] } ) ]

        nestedFlattened =
            content.nestedRules
                |> List.concatMap (flattenNestedRule parentSelectors)
    in
    currentRule ++ nestedFlattened


flattenNestedRule : List String -> Expression -> List Expression
flattenNestedRule parentSelectors expr =
    case expr of
        Rule ( childSelectors, childContent ) ->
            let
                combinedSelectors =
                    List.concatMap
                        (\parent ->
                            List.map (\child -> parent ++ " " ++ child) childSelectors
                        )
                        parentSelectors
            in
            flattenRule combinedSelectors childContent

        other ->
            [ other ]


serializeExpression : Expression -> String
serializeExpression expression =
    case expression of
        Newlines ->
            "\n"

        Comment string ->
            "/*" ++ string ++ "*/\n"

        Rule ( selectorList, content ) ->
            let
                decToCss decTuple =
                    Tuple.first decTuple ++ ":" ++ Tuple.second decTuple
            in
            if List.isEmpty content.declarations then
                ""

            else
                String.join ", " selectorList
                    ++ "{"
                    ++ String.join ";" (List.map decToCss content.declarations)
                    ++ "}\n"


{-| -}
serializeStylusAst : List Expression -> String
serializeStylusAst stylusAst =
    stylusAst
        |> List.concatMap flattenExpression
        |> List.map serializeExpression
        |> String.join ""


flattenExpression : Expression -> List Expression
flattenExpression expr =
    case expr of
        Rule ( selectorList, content ) ->
            flattenRule selectorList content

        other ->
            [ other ]


{-|

    stylusToCss """
    div
      width 400px
      height 300px
    """

yields

    div { width: 400px; height: 300px; }

-}
stylusToCss : String -> Result (List (DeadEnd Context Problem)) String
stylusToCss stylusString =
    Result.map serializeStylusAst (run stylus stylusString)
