module Component.Package.ModuleList (Model, view, isCore) where

import Graphics.Element exposing (..)
import String
import Text

import ColorScheme as C


type alias Model =
    { user : String
    , name : String
    , version : String
    , versionList : List String
    , modules : List (String, List String)
    }


toUrl : Model -> String
toUrl model =
  "/packages/" ++ model.user ++ "/" ++ model.name ++ "/" ++ model.version


isCore : Model -> Bool
isCore model =
  model.user == "elm-lang" && model.name == "core"


type SearchStatus = Yes | No


view : Int -> String -> Model -> Element
view w searchTerm model =
  let (searchStatus, modules) =
        case String.trim searchTerm of
          "" -> (No, model.modules)
          term -> (Yes, search (String.toLower term) model.modules)
  in
      case modules of
        [] -> width w (notFound searchTerm)
        _ -> flow down (List.map (viewModule w (toUrl model) searchStatus) modules)


viewModule : Int -> String -> SearchStatus -> (String, List String) -> Element
viewModule width rootUrl searchStatus (moduleName, values) =
    let url = rootUrl ++ "/" ++ String.map (\c -> if c == '.' then '-' else c) moduleName
        viewValue name =
            link width (url ++ "#" ++ name) "  " (Text.monospace (Text.fromString name))

        name = link width url "" (Text.fromString moduleName)
    in
        case searchStatus of
          No -> name
          Yes ->
            flow down (name :: spacer width 6 :: List.map viewValue values ++ [spacer width 20])


link : Int -> String -> String -> Text.Text -> Element
link width url padding txt =
  Text.monospace (Text.fromString padding) ++ Text.link url txt
    |> leftAligned
    |> container width 24 midLeft


notFound : String -> Element
notFound term =
  Text.fromString "No matches for " ++ Text.monospace (Text.fromString term)
    |> Text.color C.mediumGrey
    |> leftAligned


search : String -> List (String, List String) -> List (String, List String)
search searchTerm modules =
  List.filterMap (searchModule searchTerm) modules


searchModule : String -> (String, List String) -> Maybe (String, List String)
searchModule searchTerm (moduleName, values) =
  case List.filter (String.contains searchTerm << String.toLower) values of
    [] ->
        if String.contains searchTerm (String.toLower moduleName)
          then Just (moduleName, [])
          else Nothing

    results ->
        Just (moduleName, results)
