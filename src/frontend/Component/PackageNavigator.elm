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
        { context : Ctx.Context
        , searchDict : SearchDict
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
    = Fail Http.Error
    | Load Ctx.Context SearchDict
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

    Fail httpError ->
        ( Failed httpError
        , Fx.none
        )

    Load context searchDict ->
        ( Success
            { context = context
            , searchDict = searchDict
            , query = ""
            }
        , Fx.none
        )



-- EFFECTS


loadDocs : Ctx.Context -> Effects Action
loadDocs context =
  Ctx.getDocs context
    |> Task.map (Load context << makeSearchable)
    |> flip Task.onError (Task.succeed << Fail)
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

      Success {context, query, searchDict} ->
          [ moduleLink context Nothing
          , input
              [ placeholder "Search this package"
              , value query
              , on "input" targetValue (Signal.message addr << Query)
              ]
              []
          , viewSearchDict context query searchDict
          ]


viewSearchDict : Ctx.Context -> String -> SearchDict -> Html
viewSearchDict context query searchDict =
  if String.isEmpty query then
    ul [] (List.map (li [] << singleton << moduleLink context << Just) (Dict.keys searchDict))

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
      ul [] (List.map (viewModuleLinks context) searchResults)


viewModuleLinks : Ctx.Context -> (String, List String) -> Html
viewModuleLinks context (name, values) =
  li
    [ class "pkg-nav-search-chunk" ]
    [ moduleLink context (Just name)
    , ul [] (List.map (valueLink context name) values)
    ]


moduleLink : Ctx.Context -> Maybe String -> Html
moduleLink context name =
  let
    visibleName =
      Maybe.withDefault "README" name

    url =
      Ctx.pathTo context (Maybe.withDefault "" (Maybe.map hyphenate name))

    visibleText =
      if context.moduleName == name then
          span [ style [ "font-weight" => "bold" ] ] [ text visibleName ]

      else
          text visibleName
  in
    a [ class "pkg-nav-module", href url ] [ visibleText ]


valueLink : Ctx.Context -> String -> String -> Html
valueLink context moduleName valueName =
  li
    [ class "pkg-nav-value"
    ]
    [ a [ href (Ctx.pathTo context (hyphenate moduleName) ++ "#" ++ valueName) ] [ text valueName ]
    ]


hyphenate : String -> String
hyphenate string =
  String.map (\c -> if c == '.' then '-' else c) string


singleton : a -> List a
singleton x =
  [x]