module Parse.Combinators where

import Char
import String

import Native.Parse



-- PARSERS


type Parser a = P


run : Parser a -> String -> Result String a
run =
  Native.Parse.run



-- CHARACTERS


char : Char -> Parser Char
char c =
  satisfy ((==) c)


satisfy : (Char -> Bool) -> Parser Char
satisfy =
  Native.Parse.satisfy


lower : Parser Char
lower =
  satisfy Char.isLower


upper : Parser Char
upper =
  satisfy Char.isUpper


letter : Parser Char
letter =
  satisfy (\c -> Char.isLower c || Char.isUpper c)


digit : Parser Char
digit =
  satisfy Char.isDigit



-- STRINGS


string : String -> Parser String
string =
  Native.Parse.string


{--
int : Parser Int
int =
  Debug.crash "TODO"


float : Parser Float
float =
  Debug.crash "TODO"
--}


-- COMBINATORS


oneOf : List (Parser a) -> Parser a
oneOf =
  Native.Parse.oneOf


try : Parser a -> Parser a
try parser =
  parser


succeed : a -> Parser a
succeed =
  Native.Parse.succeed


fail : String -> Parser a
fail =
  Native.Parse.fail


map : (a -> b) -> Parser a -> Parser b
map func parser =
  parser `andThen` \a ->
    succeed (func a)


map2 : (a -> b -> c) -> Parser a -> Parser b -> Parser c
map2 func parserA parserB =
  parserA `andThen` \a ->
  parserB `andThen` \b ->
    succeed (func a b)


map3 : (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
map3 func parserA parserB parserC =
  parserA `andThen` \a ->
  parserB `andThen` \b ->
  parserC `andThen` \c ->
    succeed (func a b c)


map4 : (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
map4 func parserA parserB parserC parserD =
  parserA `andThen` \a ->
  parserB `andThen` \b ->
  parserC `andThen` \c ->
  parserD `andThen` \d ->
    succeed (func a b c d)


andThen : Parser a -> (a -> Parser b) -> Parser b
andThen =
  Native.Parse.andThen


zeroOrMore : Parser a -> Parser (List a)
zeroOrMore parser =
  oneOf
    [ parser `andThen` \a -> map ((::) a) (zeroOrMore parser)
    , succeed []
    ]


oneOrMore : Parser a -> Parser (List a)
oneOrMore parser =
  map2 (::) parser (zeroOrMore parser)


lazy : (() -> Parser a) -> Parser a
lazy thunk =
  succeed () `andThen` thunk


ignore1 : Parser x -> Parser a -> Parser a
ignore1 x parser =
  map2 (\_ a -> a) x parser


ignore2 : Parser x -> Parser y -> Parser a -> Parser a
ignore2 x y parser =
  map3 (\_ _ a -> a) x y parser


ignore3 : Parser x -> Parser y -> Parser z -> Parser a -> Parser a
ignore3 x y z parser =
  map4 (\_ _ _ a -> a) x y z parser


middle : Parser x -> Parser a -> Parser y -> Parser a
middle x parser y =
  map3 (\_ a _ -> a) x parser y
