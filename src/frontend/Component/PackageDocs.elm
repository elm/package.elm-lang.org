module Component.PackageDocs where

import Dict
import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
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
    | Readme String
    | Docs { name : String, chunks : List Chunk }


type Chunk
    = Markdown String
    | Entry Entry.Model


-- INIT


init : Ctx.Context -> (Model, Effects Action)
init context =
  ( Loading
  , getContext context
  )



-- UPDATE


type Action
    = LoadDocs String Docs.Package
    | LoadReadme String
    | Fail Http.Error


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Fail httpError ->
        ( Failed httpError
        , Fx.none
        )

    LoadReadme readme ->
        ( Readme readme
        , Fx.none
        )

    LoadDocs moduleName docs ->
        case Dict.get moduleName docs of
          Just moduleDocs ->
              ( Docs { name = moduleName, chunks = toChunks moduleDocs }
              , Fx.none
              )

          Nothing ->
              ( Failed (Http.UnexpectedPayload ("Could not find module '" ++ moduleName ++ "'"))
              , Fx.none
              )



-- EFFECTS


getContext : Ctx.Context -> Effects Action
getContext context =
  case context.moduleName of
    Nothing ->
      Ctx.getReadme context
        |> Task.map LoadReadme
        |> flip Task.onError (Task.succeed << Fail)
        |> Fx.task

    Just name ->
      Ctx.getDocs context
        |> Task.map (LoadDocs name)
        |> flip Task.onError (Task.succeed << Fail)
        |> Fx.task



-- VIEW


(=>) = (,)


view : Signal.Address Action -> Model -> Html
view addr model =
  div [ class "entry-list" ] <|
    case model of
      Loading ->
          [ p [] [text "Documentation is loading..."]
          ]

      Failed httpError ->
          [ p [] [text "Documentation did not load."]
          , p [] [text (toString httpError)]
          ]

      Readme readme ->
          [ Markdown.block readme
          ]

      Docs {name,chunks} ->
          h1 [class "entry-list-title"] [text name]
          :: List.map (viewChunk addr) chunks


viewChunk : Signal.Address Action -> Chunk -> Html
viewChunk addr chunk =
  case chunk of
    Markdown md ->
        span [class "markdown-entry"] [ Markdown.block md ]

    Entry entry ->
        Entry.view addr entry




-- MAKE CHUNKS


toChunks : Docs.Module -> List Chunk
toChunks moduleDocs =
  case String.split "\n@docs " moduleDocs.comment of
    [] ->
        Debug.crash "Expecting some documented functions in this module!"

    firstChunk :: rest ->
        Markdown firstChunk
        :: List.concatMap (subChunks moduleDocs) rest


subChunks : Docs.Module -> String -> List Chunk
subChunks moduleDocs postDocs =
    subChunksHelp moduleDocs (String.split "," postDocs)


subChunksHelp : Docs.Module -> List String -> List Chunk
subChunksHelp moduleDocs parts =
  case parts of
    [] ->
        []

    rawPart :: remainingParts ->
        let
          part =
            String.trim rawPart
        in
          case isValue part of
            Just valueName ->
              toEntry moduleDocs valueName
              :: subChunksHelp moduleDocs remainingParts

            Nothing ->
              let
                trimmedPart =
                  String.trimLeft rawPart
              in
                case String.words trimmedPart of
                  [] ->
                      [ Markdown (String.join "," parts) ]

                  token :: _ ->
                      case isValue token of
                        Just valueName ->
                          [ toEntry moduleDocs valueName
                          , Markdown (String.dropLeft (String.length token) trimmedPart)
                          ]

                        Nothing ->
                          [ Markdown (String.join "," parts) ]


var : Regex.Regex
var =
  Regex.regex "^[a-zA-Z0-9_']+$"


operator : Regex.Regex
operator =
  Regex.regex "^\\([^a-zA-Z0-9]+\\)$"


isValue : String -> Maybe String
isValue str =
  if Regex.contains var str then
    Just str

  else if Regex.contains operator str then
    Just (String.dropLeft 1 (String.dropRight 1 str))

  else
    Nothing



toEntry : Docs.Module -> String -> Chunk
toEntry moduleDocs name =
  case Dict.get name moduleDocs.entries of
    Nothing ->
        Debug.crash ("docs have been corrupted, could not find " ++ name)

    Just entry ->
        Entry entry

