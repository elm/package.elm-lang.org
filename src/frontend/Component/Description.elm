module Component.Description exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Task

import Docs.Description as Desc
import Page.Context as Ctx
import Utils.Path as Path exposing ((</>))


type Model
    = Loading
    | Failed Http.Error
    | Success
        { context : Ctx.VersionContext
        , description : Desc.Description
        }


init : Ctx.VersionContext -> (Model, Cmd Msg)
init context =
  ( Loading
  , loadDescription context
  )


-- UPDATE


type Msg
    = Fail Http.Error
    | Load Ctx.VersionContext Desc.Description


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Fail httpError ->
        ( Failed httpError
        , Cmd.none
        )

    Load context description ->
        ( Success
            { context = context
            , description = description
            }
        , Cmd.none
        )


-- EFFECTS


loadDescription : Ctx.VersionContext -> Cmd Msg
loadDescription context =
  Task.perform Fail (Load context) (Ctx.getDescription context)


-- VIEW


view : Model -> Html Msg
view model =
  div [] <|
    case model of
      Loading ->
          [ p [] [text "Loading..."]
          ]

      Failed httpError ->
          [ p [] [text "Problem loading!"]
          , p [] [text (toString httpError)]
          ]

      Success {description} ->
          [ dependencyHeader description.dependencies
          , dependencyLinks description.dependencies
          ]

dependencyHeader : List Desc.Dependency -> Html Msg
dependencyHeader dependencies =
  h2
    []
    [ text <| "Dependencies (" ++ toString (List.length dependencies) ++ ")" ]


dependencyLinks : List Desc.Dependency -> Html Msg
dependencyLinks dependencies =
  ul
    []
    <| List.map dependencyLink (List.sortBy fst dependencies)


dependencyLink : Desc.Dependency -> Html Msg
dependencyLink (name, constraint) =
  li
    [ class "pkg-nav-value" ]
    [ a
      [ href (constraintUrl name constraint) ]
      [ text name ]
    ]


constraintUrl : String -> String -> String
constraintUrl name constraint =
  "/packages" </> name </> "constraint" </> constraint