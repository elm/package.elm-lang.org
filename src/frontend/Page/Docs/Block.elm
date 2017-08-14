module Page.Docs.Block exposing
  ( view
  , Info
  , makeInfo
  )


import Dict
import Elm.Docs as Docs
import Elm.Docs.Type as Type
import Html exposing (..)
import Html.Attributes exposing (..)
import Route
import Utils.App as App
import Version



-- CONSTANTS


maxWidth : Int
maxWidth =
  64



-- MESSAGES


type alias Msg =
  Route.Route



-- VIEW


view : Info -> Docs.Block -> Html Msg
view info block =
  case block of
    Docs.MarkdownBlock markdown ->
      span [class "markdown-block"] [ Markdown.block markdown ]

    Docs.ValueBlock value ->
      viewValue info value

    Docs.AliasBlock alias ->
      viewAlias info alias

    Docs.UnionBlock union ->
      viewUnion info union

    Docs.UnknownBlock name ->
      span
        [ class "TODO-make-this-red" ]
        [ text "It seems that "
        , code [] [ text name ]
        , text " does not have any docs. Please open a bug report "
        , a [ href "https://github.com/elm-lang/package.elm-lang.org/issues" ] [ text "here" ]
        , text " with the title “UnknownBlock found in docs” and with a link to this page in the description."
        ]


viewCodeBlock : String -> String -> List (Html Msg) -> Html Msg
viewCodeBlock name comment header =
  div [ class "docs-block", id name ]
    [ div [ class "docs-header" ] header
    , div [ class "docs-comment" ] [ Markdown.block comment ]
    ]



-- VIEW VALUE BLOCK


viewValue : Info -> Docs.Value -> List (Html Msg)
viewValue info { name, comment, tipe } =
  let
    (nameTag, nameHtml) =
      valueToLink info name
  in
  viewCodeBlock nameTag comment <|
    case toLines tipe of
      One _ line ->
        [ nameHtml :: padded colon :: line ]

      More x xs ->
        (nameHtml :: padded colon) :: indentFour x :: List.map indentFour xs


indentFour : Line msg -> Line msg
indentFour =
  (::) (text "    ")



-- VIEW ALIAS BLOCK


viewAlias : Info -> Docs.Alias -> List (Html Msg)
viewAlias info { name, vars, comment, tipe } =
  let
    varsString =
      String.concat (List.map ((++) " ") vars)

    aliasNameLine =
      [ keyword "type", space, keyword "alias", space
      , typeToLink info name, text varsString, space
      , equals
      ]
  in
  viewCodeBlock name comment <|
    aliasNameLine :: List.map indentFour (linesToList (toLines tipe))



-- VIEW UNION


viewUnion : Info -> Docs.Union -> List (Html Msg)
viewUnion info {name, comment, vars, tags} =
  let
    varsString =
      String.concat <| List.map ((++) " ") vars

    nameLine =
      [ keyword "type", space, typeToLink info name, text varsString ]
  in
  viewCodeBlock name comment <| More nameLine <|
    case tags of
      [] ->
        []

      t :: ts ->
        linesToList (toMoreLines (unionMore info) t ts)


unionMore : Info -> MoreSettings (String, List Type.Type) msg
unionMore info =
  let
    ctorToLines (ctor,args) =
      toOneOrMore (toLines info Other (Type.Type ctor args))
  in
  { open = [ text "    = "]
  , sep = text "    | "
  , close = Nothing
  , openIndent = 6
  , sepIndent = 6
  , toLines = ctorToLines
  }



-- INFO


type alias Info =
  { user : String
  , project : String
  , version : Version.Version
  , moduleName : String
  , typeNameDict : TypeNameDict
  }


type alias TypeNameDict =
  Dict.Dict String (String, String)


makeInfo : String -> String -> Version.Version -> String -> List Docs.Module -> Info
makeInfo user project version moduleName docsList =
  let
    addUnion moduleName union docs =
      Dict.insert (moduleName ++ "." ++ union.name) (moduleName, union.name) docs

    addModule docs dict =
      List.foldl (addUnion docs.name) dict docs.unions
  in
    Info user project version moduleName <|
      List.foldl addModule Dict.empty docsList



-- CREATE LINKS


typeToLink : Info -> String -> Html Msg
typeToLink info name =
  makeLink info [bold] name name


valueToLink : Info -> Docs.Name -> ( String, Html Msg )
valueToLink info valueName =
  case valueName of
    Docs.Name name ->
      makeLink info [bold] name name

    Docs.Op name _ _ ->
      makeLink info [bold] name <| "(" ++ name ++ ")"


bold : Attribute msg
bold =
  style "font-weight" "bold"


makeLink : Info -> List (Attribute Msg) -> String -> String -> Html Msg
makeLink {user, project, version, moduleName} attrs tagName humanName =
  let
    route =
      Route.Module user project (Route.Exactly version) moduleName (Just tagName)
  in
  App.link identity route attrs [ text humanName ]


toLinkLine : Info -> String -> Line Msg
toLinkLine info qualifiedName =
  case Dict.get qualifiedName info.typeNameDict of
    Nothing ->
      let
        shortName =
          last qualifiedName (String.split "." qualifiedName)
      in
      One (String.length shortName) (span [ title qualifiedName ] [ text shortName ])

    Just (moduleName, name) ->
      One (String.length name) (makeLink { info | moduleName = moduleName } [] name name)


last : a -> List a -> a
last backup list =
  case list of
    [] ->
      backup

    x :: [] ->
      x

    _ :: xs ->
      last backup xs



-- LINES


type alias Line msg =
  List (Html msg)


type Lines line
  = One Int line
  | More line (List line)


type Context = Func | App | Other


toLines : Info -> Context -> Type.Type -> Lines (Line msg)
toLines info context tipe =
  case tipe of
    Type.Var x ->
      One (String.length x) [text x]

    Type.Lambda arg result ->
      let
        lambdaToLine =
          if context == Other then
            toLinesHelp lambdaOne lambdaMore
          else
            toLinesHelp lambdaOneParens lambdaMoreParens
      in
      lambdaToLine (toLines info Func arg) <|
        List.map (toLines info Func) (collectArgs [] result)

    Type.Tuple [] ->
      One 2 [text "()"]

    Type.Tuple (arg :: args) ->
      toLinesHelp tupleOne tupleMore <|
        toLines info Other arg
        :: List.map (toLines info Other) args

    Type.Type name args ->
      toLinesHelp
        (typeOne (context == App))
        (typeMore (context == App))
        (toLinkLine info name)
        (List.map (toLines info App) args)

    Type.Record [] Nothing ->
      One 2 [text "{}"]

    Type.Record [] (Just ext) ->
      One (6 + String.length ext) [ text <| "{ " ++ ext ++ " | }" ]

    Type.Record (f :: fs) extension ->
      let
        toLns ( field, tipe ) =
          ( field, toLines info Other tipe )
      in
      case extension of
        Nothing ->
          toLinesHelp recordOne recordMore (toLns f) (List.map toLns fs)

        Just ext ->
          case toLinesHelp (recordOneExt ext) recordMoreExt (toLns f) (List.map toLns fs) of
            One width line ->
              One width line

            More first rest ->
              More [ text "{ ", text ext ] (first :: rest ++ [ [ text "}" ] ])



-- FUNCTIONS


collectArgs : List Type.Type -> Type.Type -> List Type.Type
collectArgs revArgs tipe =
  case tipe of
    Type.Lambda arg result ->
      collectArgs (arg :: revArgs) result

    _ ->
      List.reverse (tipe :: revArgs)


lambdaOne : OneSettings (Lines (Line msg)) msg
lambdaOne =
  { open = []
  , sep = text " -> "
  , close = []
  , openWidth = 0
  , sepWidth = 2
  , closeWidth = 0
  , toLine = toLine
  }


lambdaMore : MoreSettings (Lines (Line msg)) msg
lambdaMore =
  { open = []
  , sep = text "-> "
  , close = Nothing
  , openIndent = 0
  , sepIndent = 3
  , toLines = toOneOrMore
  }


lambdaOneParens : OneSettings (Lines (Line msg)) msg
lambdaOneParens =
  { open = [ text "(" ]
  , sep = text " -> "
  , close = [ text ")" ]
  , openWidth = 1
  , sepWidth = 2
  , closeWidth = 1
  , toLine = toLine
  }


lambdaMoreParens : MoreSettings (Lines (Line msg)) msg
lambdaMoreParens =
  { open = [ text "( " ]
  , sep = text "  -> "
  , close = Just [ text ")" ]
  , openIndent = 2
  , sepIndent = 5
  , toLines = toOneOrMore
  }



-- TUPLES


tupleOne : OneSettings (Lines (Line msg)) msg
tupleOne =
  { open = [ text "( " ]
  , sep = text ", "
  , close = [ text " )" ]
  , openWidth = 2
  , sepWidth = 2
  , closeWidth = 2
  , toLine = toLine
  }


tupleMore : MoreSettings (Lines (Line msg)) msg
tupleMore =
  { open = [ text "( " ]
  , sep = text ", "
  , close = Just [text ")"]
  , openIndent = 2
  , sepIndent = 2
  , toLines = toOneOrMore
  }



-- TYPES


typeOne : Bool -> OneSettings (Lines (Line msg)) msg
typeOne needsParens =
  if needsParens then
    { open = [ text "(" ]
    , sep = text " "
    , close = [ text ")" ]
    , openWidth = 1
    , sepWidth = 1
    , closeWidth = 1
    , toLine = toLine
    }

  else
    { open = []
    , sep = text " "
    , close = []
    , openWidth = 0
    , sepWidth = 1
    , closeWidth = 0
    , toLine = toLine
    }


typeMore : Bool -> MoreSettings (Lines (Line msg)) msg
typeMore needsParens =
  if needsParens then
    { open = [text "("]
    , sep = text "    "
    , close = Just [text ")"]
    , openIndent = 0
    , sepIndent = 4
    , toLines = toOneOrMore
    }

  else
    { open = []
    , sep = text "    "
    , close = Nothing
    , openIndent = 0
    , sepIndent = 4
    , toLines = toOneOrMore
    }



-- RECORDS


recordOne : OneSettings (String, Lines (Line msg)) msg
recordOne =
  { open = [ text "{ " ]
  , sep = text ", "
  , close = [ text " }" ]
  , openWidth = 2
  , sepWidth = 2
  , closeWidth = 2
  , toLine = fieldToLine
  }


recordMore : MoreSettings (String, Lines (Line msg)) msg
recordMore =
  { open = [ text "{ " ]
  , sep = text ", "
  , indent = 6
  , close = Just [text "}"]
  , toLines = fieldToLines
  }



-- EXTENDED RECORDS


recordOneExt : String -> OneSettings (String, Lines (Line msg)) msg
recordOneExt extension =
  let
    open =
      "{ " ++ extension ++ " | "
  in
  { open = [ text open ]
  , sep = text ", "
  , close = [ text " }" ]
  , openWidth = String.length open
  , sepWidth = 2
  , closeWidth = 2
  , toLine = fieldToLine
  }


recordMoreExt : MoreSettings (String, Lines (Line msg)) msg
recordMoreExt =
  { open = [ text "    | " ]
  , sep = text "    , "
  , indent = 10
  , close = Nothing
  , toLines = fieldToLines
  }



-- RECORD HELPERS


fieldToLine : (String, Lines (Line msg)) -> Maybe (Int, Line msg)
fieldToLine ( field, lines ) =
  case lines of
    More _ _ ->
      Nothing

    One width line ->
      Just ( String.length field + 3 + width, text field :: text " : " :: line )


fieldToLines : (String, Lines (Line msg)) -> OneOrMore (Line msg)
fieldToLines ( field, lines ) =
  case lines of
    One width line ->
      let
        potentialWidth =
          String.length field + 3 + width
      in
      if potentialWidth < maxWidth then
        OneOrMore [ text field :: text " : " :: line ] []
      else
        OneOrMore [ text field :: text " :" ] [ line ]

    More x xs ->
      OneOrMore [ text field, text " :" ] (x :: xs)



-- HELPERS


toLine : Lines line -> Maybe (Int, line)
toLine lines =
  case lines of
    One width line ->
      Just ( width, line )

    More _ _ ->
      Nothing


linesToList : Lines line -> List line
linesToList lines =
  case lines of
    One _ line ->
      [line]

    More x xs ->
      x :: xs


type OneOrMore a =
  OneOrMore a (List a)


toOneOrMore : Lines line -> OneOrMore line
toOneOrMore lines =
  case lines of
    One _ line ->
      OneOrMore line []

    More x xs ->
      OneOrMore x xs



-- TO LINES HELP


type alias OneSettings a msg =
  { open : Line msg
  , sep : Html msg
  , close : Line msg
  , openWidth : Int
  , sepWidth : Int
  , closeWidth : Int
  , toLine : a -> Maybe (Int, Line msg)
  }


type alias MoreSettings a msg =
  { open : Line msg
  , sep : Html msg
  , close : Maybe (Line msg)
  , openIndent : Int
  , sepIndent : Int
  , toLines : a -> OneOrMore (Line msg)
  }


toLinesHelp : OneSettings a msg -> MoreSettings a msg -> List a -> Lines (Line msg)
toLinesHelp one more x xs =
  let
    maybeOneLine =
      case one.ends of
        Nothing ->
          toOneLine "" one.sep Nothing one.toLine (x::xs)

        Just (open, close) ->
          toOneLine open one.sep (Just close) one.toLine (x::xs)
  in
  case maybeOneLine of
    Just ( width, line ) ->
      One width line

    Nothing ->
      toMoreLines more x xs


toOneLine : String -> String -> Maybe String -> (a -> Maybe (Int, Line msg)) -> List a -> Maybe (Int, Line msg)
toOneLine start sep maybeClose toLine entries =
  case entries of
    [] ->
      case maybeClose of
        Nothing ->
          Just ( 0, [] )

        Just close ->
          Just ( String.length close, [ text close ] )

    entry :: remainingEntries ->
      case toLine entry of
        Nothing ->
          Nothing

        Just (entryWidth, line) ->
          case toOneLine sep sep maybeClose toLine remainingEntries of
            Nothing ->
              Nothing

            Just (remainingWidth, remainingLine) ->
              let
                width =
                  String.length start + entryWidth + remainingWidth
              in
              if width < maxWidth then
                Just ( width, text start :: line ++ remainingLine )
              else
                Nothing


toMoreLines : MoreSettings a msg -> a -> List a -> Lines (Line msg)
toMoreLines {open, sep, indent, close, toLines} x xs =
  toMoreLinesHelp open sep indent close (toLines x) (List.map toLines xs)


toMoreLinesHelp : Line msg -> Html msg -> Int -> Maybe (Line msg) -> OneOrMore (Line msg) -> List (OneOrMore (Line msg)) -> Lines (Line msg)
toMoreLinesHelp open sep indent close (OneOrMore firstLine firstRest) others =
  let
    indentation =
      text (String.repeat indent ' ')

    toChunk (OneOrMore x xs) =
      (sep :: x) :: List.map ((::) indentation) xs

    otherLines =
      List.map ((::) indentation) firstRest
      ++ List.concatMap toChunk others
  in
    More (open ++ firstLine) <|
      case close of
        Nothing ->
          otherLines

        Just closer ->
          otherLines ++ [ close ]
