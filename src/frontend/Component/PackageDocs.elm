module Component.PackageDocs exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Process
import Regex
import Set
import String
import Task

import Docs.Entry as Entry
import Docs.Name as Name
import Docs.Package as Docs
import Docs.Type as Type
import Native.Jump
import Page.Context as Ctx
import Parse.Type as Type
import Utils.Markdown as Markdown



-- MODEL


type Model
    = Loading
    | Failed String
    | Readme String
    | RawDocs (Info String)
    | ParsedDocs (Info Type.Type)


type alias Info tipe =
  { name : String
  , nameDict : Name.Dictionary
  , chunks : List (Chunk tipe)
  }


type Chunk tipe
    = Markdown String
    | Entry (Entry.Model tipe)


-- INIT


init : Ctx.VersionContext -> (Model, Cmd Msg)
init context =
  ( Loading
  , getContext context
  )



-- UPDATE


type Msg
    = LoadDocs String (Result Http.Error Docs.Package)
    | LoadParsedDocs (List (Chunk Type.Type))
    | LoadReadme (Result Http.Error String)
    | NoOp


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
        ( model
        , Cmd.none
        )

    LoadReadme (Err _) ->
        ( Failed "The README did not load."
        , Cmd.none
        )

    LoadReadme (Ok readme) ->
        ( Readme readme
        , Cmd.none
        )

    LoadDocs _ (Err _) ->
        ( Failed "The documentation did not load."
        , Cmd.none
        )

    LoadDocs moduleName (Ok docs) ->
        case Dict.get moduleName docs of
          Just moduleDocs ->
              let
                chunks =
                  toChunks moduleDocs
              in
                ( RawDocs (Info moduleName (toNameDict docs) chunks)
                , delayedTypeParse chunks
                )

          Nothing ->
              ( Failed ("Could not find module '" ++ moduleName ++ "'")
              , Cmd.none
              )

    LoadParsedDocs newChunks ->
        case model of
          RawDocs info ->
              ( ParsedDocs { info | chunks = newChunks }
              , jumpToHash
              )

          _ ->
              ( Failed "Something went wrong parsing types."
              , Cmd.none
              )


toNameDict : Docs.Package -> Name.Dictionary
toNameDict pkg =
  Dict.map (\_ modul -> Set.fromList (Dict.keys modul.entries)) pkg



-- EFFECTS


getContext : Ctx.VersionContext -> Cmd Msg
getContext context =
  case context.moduleName of
    Nothing ->
      Http.send LoadReadme (Ctx.getReadme context)

    Just name ->
      Http.send (LoadDocs name) (Ctx.getDocs context)


delayedTypeParse : List (Chunk String) -> Cmd Msg
delayedTypeParse chunks =
  Task.perform LoadParsedDocs <|
    Task.succeed (List.map (chunkMap stringToType) chunks)


chunkMap : (a -> b) -> Chunk a -> Chunk b
chunkMap func chunk =
  case chunk of
    Markdown md ->
      Markdown md

    Entry entry ->
      Entry (Entry.map func entry)


stringToType : String -> Type.Type
stringToType str =
  case Type.parse str of
    Ok tipe ->
      tipe

    Err _ ->
      Type.Var str


jumpToHash : Cmd Msg
jumpToHash =
  Task.perform (always NoOp) (
    Process.sleep 0
      |> Task.andThen (\_ -> Native.Jump.jump)
  )



-- VIEW


(=>) = (,)


view : Model -> Html msg
view model =
  div [ class "entry-list" ] <|
    case model of
      Loading ->
          [ p [] [text "Loading..."]
          ]

      Failed httpError ->
          [ p [] [text "Documentation did not load."]
          , p [] [text (toString httpError)]
          ]

      Readme readme ->
          [ Markdown.block readme
          ]

      RawDocs {name,chunks} ->
          h1 [class "entry-list-title"] [text name]
          :: List.map (viewChunk Entry.stringView) chunks

      ParsedDocs {name,nameDict,chunks} ->
          h1 [class "entry-list-title"] [text name]
          :: List.map (viewChunk (Entry.typeView nameDict)) chunks


viewChunk : (Entry.Model tipe -> Html msg) -> Chunk tipe -> Html msg
viewChunk entryView chunk =
  case chunk of
    Markdown md ->
        span [class "markdown-entry"] [ Markdown.block md ]

    Entry entry ->
        entryView entry




-- MAKE CHUNKS


toChunks : Docs.Module -> List (Chunk String)
toChunks moduleDocs =
  case String.split "\n@docs " moduleDocs.comment of
    [] ->
        Debug.crash "Expecting some documented functions in this module!"

    firstChunk :: rest ->
        Markdown firstChunk
        :: List.concatMap (subChunks moduleDocs) rest


subChunks : Docs.Module -> String -> List (Chunk String)
subChunks moduleDocs postDocs =
    case Regex.split (Regex.AtMost 1) (Regex.regex "\n") postDocs of
      [] ->
        Debug.crash "Expected a newline between @docs statements!"

      firstChunk :: rest ->
        let
          handleRest =
            if String.endsWith "," (String.trimRight firstChunk) then
              multiLineDocsChunk moduleDocs
            else
              List.map Markdown
        in
          subChunksHelp moduleDocs (String.split "," firstChunk) ++ handleRest rest


multiLineDocsChunk : Docs.Module -> List String -> List (Chunk String)
multiLineDocsChunk moduleDocs docsToParse =
  let
    errMsg =
      "Found an @docs listing that ended with a comma, but found no more docs on the next line!"
  in
    case docsToParse of
      [] ->
        Debug.crash errMsg

      [rest] ->
        if String.startsWith "\n" rest || String.startsWith "#" rest then
          Debug.crash errMsg
        else
          subChunks moduleDocs rest

      _ ->
        Debug.crash "This shouldn't happen: the Regex was only supposed to take one line"


subChunksHelp : Docs.Module -> List String -> List (Chunk String)
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
                          , trimmedPart :: remainingParts
                              |> String.join ","
                              |> String.dropLeft (String.length token)
                              |> Markdown
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



toEntry : Docs.Module -> String -> Chunk String
toEntry moduleDocs name =
  case Dict.get name moduleDocs.entries of
    Nothing ->
        Debug.crash ("docs have been corrupted, could not find " ++ name)

    Just entry ->
        Entry entry
