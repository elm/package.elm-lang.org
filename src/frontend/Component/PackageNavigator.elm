module Component.PackageNavigator where

import Dict
import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Regex
import String
import Task

import Docs.Package as Docs
import Docs.Entry as Entry
import Page.Context as Ctx
import Utils.Markdown as Markdown


type Model
    = Loading
    | Failed Http.Error
    | Success
        { searchDict : SearchDict
        , query : String
        }


type alias SearchDict =
    Dict.Dict String (List String)



-- INIT


init : Ctx.Context -> (Model, Effects Action)
init context =
  ( Loading
  , loadDocs context
  )



-- UPDATE


type Action
    = Load (Result Http.Error SearchDict)
    | Query String


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Query query ->
      flip (,) Fx.none <|
        case model of
          Success facts ->
              Success { facts | query = query }

          Loading ->
              model

          Failed err ->
              model

    Load (Err httpError) ->
        ( Failed httpError
        , Fx.none
        )

    Load (Ok searchDict) ->
        ( Success { searchDict = searchDict, query = "" }
        , Fx.none
        )



-- EFFECTS


loadDocs : Ctx.Context -> Effects Action
loadDocs context =
  Ctx.getDocs context
    |> Task.map makeSearchable
    |> Task.toResult
    |> Task.map Load
    |> Fx.task


makeSearchable : Docs.Package -> SearchDict
makeSearchable pkg =
  Dict.map (\_ modul -> Dict.keys modul.entries) pkg



-- VIEW


(=>) = (,)


view : Signal.Address Action -> Model -> Html
view addr model =
  div [class "pkg-nav"] <|
    case model of
      Loading ->
          [ p [] [text "Loading..."]
          ]

      Failed httpError ->
          [ p [] [text "Problem loading!"]
          , p [] [text (toString httpError)]
          ]

      Success {query, searchDict} ->
          [ a [ class "pkg-nav-module", href "README" ] [ text "README" ]
          , input
              [ placeholder "Search this package"
              , value query
              , on "input" targetValue (Signal.message addr << Query)
              ]
              []
          , viewSearchDict query searchDict
          ]


viewSearchDict : String -> SearchDict -> Html
viewSearchDict query searchDict =
  if String.isEmpty query then
    ul [] (List.map (li [] << singleton << moduleLink) (Dict.keys searchDict))

  else
    let
      lowerQuery =
        String.toLower query

      containsQuery value =
        String.contains lowerQuery (String.toLower value)

      searchResults =
        Dict.map (\_ values -> List.filter containsQuery values) searchDict
          |> Dict.filter (\_ values -> not (List.isEmpty values))
          |> Dict.toList
    in
      ul [] (List.map viewModuleLinks searchResults)


viewModuleLinks : (String, List String) -> Html
viewModuleLinks (name, values) =
  li [ class "pkg-nav-search-chunk" ]
    [ moduleLink name
    , ul [] (List.map (valueLink name) values)
    ]


moduleLink : String -> Html
moduleLink name =
  a
    [ class "pkg-nav-module"
    , href (hyphenate name)
    ]
    [ text name ]


valueLink : String -> String -> Html
valueLink moduleName valueName =
  li
    [ class "pkg-nav-value"
    ]
    [ a [ href (hyphenate moduleName ++ "#" ++ valueName) ] [ text valueName ]
    ]


hyphenate : String -> String
hyphenate string =
  String.map (\c -> if c == '.' then '-' else c) string


singleton : a -> List a
singleton x =
  [x]