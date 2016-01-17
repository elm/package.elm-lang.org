module Component.PackageSidebar where

import Dict
import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Set
import String
import Task

import Docs.Package as Docs
import Docs.Entry as Entry
import Page.Context as Ctx
import Utils.Path as Path exposing ((</>))


type Model
    = Loading
    | Failed Http.Error
    | Success
        { context : Ctx.VersionContext
        , searchDict : SearchDict
        , query : String
        }


type alias SearchDict =
    Dict.Dict String (List LinkInfo)


type alias LinkInfo =
    { name : String
    , owner : String
    }


-- INIT


init : Ctx.VersionContext -> (Model, Effects Action)
init context =
  ( Loading
  , loadDocs context
  )



-- UPDATE


type Action
    = Fail Http.Error
    | Load Ctx.VersionContext SearchDict
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


loadDocs : Ctx.VersionContext -> Effects Action
loadDocs context =
  Ctx.getDocs context
    |> Task.map (Load context << toSearchDict)
    |> flip Task.onError (Task.succeed << Fail)
    |> Fx.task


toSearchDict : Docs.Package -> SearchDict
toSearchDict pkg =
  Dict.map toLinkInfo pkg


toLinkInfo : String -> Docs.Module -> List LinkInfo
toLinkInfo _ modul =
  let
    entryNames =
      Dict.keys modul.entries

    nameSet =
      Set.fromList entryNames

    tagInfo =
      Dict.values modul.entries
        |> List.concatMap (gatherTagInfo nameSet)

    topLevelInfo =
      List.map (\name -> LinkInfo name name) entryNames
  in
    tagInfo ++ topLevelInfo


gatherTagInfo : Set.Set String -> Entry.Model t -> List { name : String, owner : String }
gatherTagInfo topLevelNames entry =
  let
    toNamePair {tag} =
      if Set.member tag topLevelNames then
        Nothing

      else
        Just (LinkInfo tag entry.name)
  in
    case entry.info of
      Entry.Union {tags} ->
        List.filterMap toNamePair tags

      _ ->
        []



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
          , br [] []
          , githubLink context
          , h2 [] [ text "Module Docs" ]
          , input
              [ placeholder "Search"
              , value query
              , on "input" targetValue (Signal.message addr << Query)
              ]
              []
          , viewSearchDict context query searchDict
          ]


viewSearchDict : Ctx.VersionContext -> String -> SearchDict -> Html
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
        searchDict
          |> Dict.map (\_ values -> List.filter (.name >> containsQuery) values)
          |> Dict.filter (\key values -> not (List.isEmpty values) || containsQuery key)
          |> Dict.toList
    in
      ul [] (List.map (viewModuleLinks context) searchResults)


viewModuleLinks : Ctx.VersionContext -> (String, List LinkInfo) -> Html
viewModuleLinks context (name, values) =
  li
    [ class "pkg-nav-search-chunk" ]
    [ moduleLink context (Just name)
    , ul [] (List.map (valueLink context name) values)
    ]


githubLink : Ctx.VersionContext -> Html
githubLink context =
  a [ class "pkg-nav-module"
    , href ("https://github.com" </> context.user </> context.project </> "tree" </> context.version)
    ]
    [ text "Browse source" ]


moduleLink : Ctx.VersionContext -> Maybe String -> Html
moduleLink context name =
  let
    visibleName =
      Maybe.withDefault "README" name

    url =
      Ctx.pathTo context (Maybe.withDefault "" (Maybe.map Path.hyphenate name))

    visibleText =
      if context.moduleName == name then
          span [ style [ "font-weight" => "bold", "text-decoration" => "underline" ] ] [ text visibleName ]

      else
          text visibleName
  in
    a [ class "pkg-nav-module", href url ] [ visibleText ]


valueLink : Ctx.VersionContext -> String -> LinkInfo -> Html
valueLink context moduleName {name, owner} =
  let
    url =
      Ctx.pathTo context (Path.hyphenate moduleName) ++ "#" ++ owner
  in
    li
      [ class "pkg-nav-value"
      ]
      [ a [ href url ] [ text name ]
      ]


singleton : a -> List a
singleton x =
  [x]
