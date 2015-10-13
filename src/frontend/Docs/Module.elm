module Docs.Module where

import Debug
import Dict
import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Regex
import String
import Task

import Docs
import Docs.Decoder as Decode
import Docs.Entry as Entry
import Utils.Markdown as Markdown


type Model
    = Loading
    | Failed Http.Error
    | Success { name : String, chunks : List Chunk }


type Chunk
    = Markdown String
    | Entry Entry.Model


-- INIT


type alias Context =
    { user : String
    , project : String
    , version : String
    , module_ : String
    }


init : Context -> (Model, Effects Action)
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


(</>) a b =
    a ++ "/" ++ b


loadDocs : Context -> Effects Action
loadDocs {user,project,version,module_} =
  let
    get =
      Http.get Decode.module_ ("/packages" </> user </> project </> version </> "docs" </> hyphenate module_ ++ ".json")
  in
    Fx.task (Task.map Load (Task.toResult get))


hyphenate : String -> String
hyphenate str =
  String.map (\c -> if c == '.' then '-' else c) str


-- VIEW


(=>) = (,)


view : Signal.Address Action -> Model -> Html
view addr model =
  div [class "center"] <|
    case model of
      Loading ->
          [ p [] [text "Documentation is loading..."]
          ]

      Failed httpError ->
          [ p [] [text "Documentation did not load."]
          , p [] [text (toString httpError)]
          ]

      Success {name,chunks} ->
          [ div [ class "entry-list" ] <|
              h1 [class "entry-list-title"] [text name]
              :: List.map (viewChunk addr) chunks
          ]


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
          if Regex.contains var part then
              toEntry moduleDocs part
              :: subChunksHelp moduleDocs remainingParts

          else
              let
                trimmedPart =
                  String.trimLeft rawPart
              in
                case String.words trimmedPart of
                  [] ->
                      [ Markdown (String.join "," parts) ]

                  token :: _ ->
                      if Regex.contains var token then
                          [ toEntry moduleDocs token
                          , Markdown (String.dropLeft (String.length token) trimmedPart)
                          ]

                      else
                          [ Markdown (String.join "," parts) ]


var : Regex.Regex
var =
  Regex.regex "^[a-zA-Z0-9_']+$"


toEntry : Docs.Module -> String -> Chunk
toEntry moduleDocs name =
  case Dict.get name moduleDocs.entries of
    Nothing ->
        Debug.crash "docs have been corrupted"

    Just entry ->
        Entry entry
