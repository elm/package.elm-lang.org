module Component.ModuleDocs where

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
    | Success { name : String, chunks : List Chunk }


type Chunk
    = Markdown String
    | Entry Entry.Model


-- INIT


init : Ctx.Context -> (Model, Effects Action)
init context =
  ( Loading
  , loadDocs context
  )



-- UPDATE


type Action
    = Load (Result Http.Error Docs.Module)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Load (Err httpError) ->
        ( Failed httpError
        , Fx.none
        )

    Load (Ok docs) ->
        ( Success { name = docs.name, chunks = toChunks docs }
        , Fx.none
        )



-- EFFECTS


loadDocs : Ctx.Context -> Effects Action
loadDocs context =
  Ctx.getDocs context
    |> Task.toResult
    |> Task.map (\r -> Load (r `Result.andThen` getModule (Maybe.withDefault "" context.moduleName)))
    |> Fx.task


getModule : String -> Docs.Package -> Result Http.Error Docs.Module
getModule moduleName pkg =
  case Dict.get moduleName pkg of
    Just moduleDocs ->
        Ok moduleDocs

    Nothing ->
        Err (Http.UnexpectedPayload ("Could not find module '" ++ moduleName ++ "'"))



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

      Success {name,chunks} ->
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

