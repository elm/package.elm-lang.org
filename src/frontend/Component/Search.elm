module Component.Search where

import Dict
import Effects as Fx exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json
import Regex
import Set
import String
import Task

import Docs.Summary as Summary
import Docs.Entry as Entry
import Docs.Name as Name
import Docs.Package as Docs
import Docs.Type as Type
import Page.Context as Ctx
import Parse.Type as Type



-- MODEL


type Model
    = Loading
    | Failed Http.Error
    | Catalog (List Summary.Summary)
    | RawDocs (Info String)
    | ParsedDocs (Info Type.Type)


type alias Info tipe =
  { name : String
  , nameDict : Name.Dictionary
  , chunks : List (Chunk tipe)
  , query : String
  }


type alias Chunk tipe
    = (Name.Canonical, Entry.Model tipe)


-- INIT


init : (Model, Effects Action)
init =
  ( Loading
  , getPackageInfo
  )



-- UPDATE


type Action
    = LoadCatalog (List Summary.Summary, List String)
    | LoadDocs Docs.Package
    | LoadParsedDocs (List (Chunk Type.Type))
    | Fail Http.Error
    | Query String
    | NoOp


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    NoOp ->
        ( model
        , Fx.none
        )

    Query query ->
        flip (,) Fx.none <|
          case model of
            ParsedDocs info ->
                ParsedDocs { info | query = query }

            Catalog _ ->
                model

            RawDocs _ ->
                model

            Loading ->
                model

            Failed err ->
                model

    Fail httpError ->
        ( Failed httpError
        , Fx.none
        )

    LoadCatalog (allSummaries, updatedPkgs) ->
        let
          updatedSet =
            Set.fromList updatedPkgs

          (summaries, oldSummaries) =
            List.partition (\{name} -> Set.member name updatedSet) allSummaries

          contextEffects = summaries
            |> List.map latestVersionContext
            |> List.map getContext

        in
          ( Catalog summaries
          , Fx.batch contextEffects
          )

    LoadDocs docs ->
        let
          chunkEffects = docs
            |> Dict.toList
            |> List.map (\ (_, moduleDocs) -> delayedTypeParse (toChunks moduleDocs))

        in
          ( RawDocs (Info "" (toNameDict docs) [] "")
          , Fx.batch chunkEffects
          )

    LoadParsedDocs newChunks ->
        case model of
          RawDocs info ->
              ( ParsedDocs { info | chunks = newChunks }
              , Fx.none
              )

          ParsedDocs info ->
              ( ParsedDocs { info | chunks = info.chunks ++ newChunks }
              , Fx.none
              )

          _ ->
              ( Failed (Http.UnexpectedPayload ("Something went wrong parsing types."))
              , Fx.none
              )


toNameDict : Docs.Package -> Name.Dictionary
toNameDict pkg =
  Dict.map (\_ modul -> Set.fromList (Dict.keys modul.entries)) pkg


latestVersionContext : Summary.Summary -> Ctx.VersionContext
latestVersionContext summary =
  let
    userProject = String.split "/" summary.name
    user = Maybe.withDefault "user" (List.head userProject)
    project = Maybe.withDefault "project" (List.head (List.reverse userProject))
    version = List.head summary.versions
      |> Maybe.withDefault (1,0,0)
      |> (\ (a,b,c) -> String.join "." (List.map toString [a,b,c]))
    allVersions = []
  in
    Ctx.VersionContext
      user
      project
      version
      allVersions
      Nothing


-- EFFECTS


getPackageInfo : Effects Action
getPackageInfo =
  let
    getAll =
      Http.get Summary.decoder "/all-packages"

    getNew =
      Http.get (Json.list Json.string) "/new-packages"

  in
    Task.map2 (,) getAll getNew
      |> Task.map LoadCatalog
      |> flip Task.onError (Task.succeed << Fail)
      |> Fx.task


getContext : Ctx.VersionContext -> Effects Action
getContext context =
  Ctx.getDocs context
    |> Task.map LoadDocs
    |> flip Task.onError (Task.succeed << Fail)
    |> Fx.task


delayedTypeParse : List (Chunk String) -> Effects Action
delayedTypeParse chunks =
  Fx.task <|
    Task.succeed () `Task.andThen` \_ ->
        Task.succeed (LoadParsedDocs (List.map (chunkMap stringToType) chunks))


chunkMap : (a -> b) -> Chunk a -> Chunk b
chunkMap func (name, entry) =
  (name, Entry.map func entry)


stringToType : String -> Type.Type
stringToType str =
  case Type.parse str of
    Ok tipe ->
      tipe

    Err _ ->
      Type.Var str



-- VIEW


(=>) = (,)


view : Signal.Address Action -> Model -> Html
view addr model =
  div [class "search"] <|
    case model of
      Loading ->
          [ p [] [text "Loading list of packages..."]
          ]

      Catalog catalog ->
          [ p [] [text <| "Loading docs for " ++ toString (List.length catalog) ++ "packages..."]
          ]

      Failed httpError ->
          [ p [] [text "Documentation did not load or parse."]
          , p [] [text (toString httpError)]
          ]

      RawDocs {name,chunks} ->
          [ p [] [text "Parsing..."]
          ]

      ParsedDocs {name,nameDict,chunks,query} ->
          input
            [ placeholder "Search function by name or type"
            , value query
            , on "input" targetValue (Signal.message addr << Query)
            ]
            []
          :: viewSearchResults addr nameDict query chunks


viewSearchResults : Signal.Address Action -> Name.Dictionary -> String -> List (Chunk Type.Type) -> List Html
viewSearchResults addr nameDict query chunks =
  let
    queryType = stringToType query

  in
    if String.isEmpty query then
      [ h1 [] [ text "Welcome to Elm Search" ]
      , p [] [ text "Search the latest Elm libraries by either function name, or by approximate type signature."]
      , h2 [] [ text "Example searches" ]
      , ul []
        [ li [] [ a [ onClick addr (Query "map")] [ text "map" ] ]
          , li [] [ a [ onClick addr (Query "(a -> b -> b) -> b -> List a -> b")] [ text "(a -> b -> b) -> b -> List a -> b" ] ]        ]
      ]

    else
      case queryType of
        Type.Var string ->
            chunks
              |> List.filter (\ (name, entry) -> Entry.typeContainsQuery query entry)
              |> List.map (\ (name, entry) -> Entry.typeViewAnnotation name nameDict entry)

        _ ->
            chunks
              -- TODO: clean this up
              |> List.map (\ (name, entry) -> (Entry.typeSimilarity queryType entry, (name, entry)))
              |> List.filter (\ (similarity, _) -> similarity > 1)
              |> List.sortBy (\ (similarity, _) -> -similarity)
              |> List.map (\ (_, chunk) -> chunk)
              |> List.map (\ (name, entry) -> Entry.typeViewAnnotation name (Dict.filter (\ key _ -> key == name.home) nameDict) entry)



-- MAKE CHUNKS


toChunks : Docs.Module -> List (Chunk String)
toChunks moduleDocs =
  case String.split "\n@docs " moduleDocs.comment of
    [] ->
        Debug.crash "Expecting some documented functions in this module!"

    firstChunk :: rest ->
        List.concatMap (subChunks moduleDocs) rest


subChunks : Docs.Module -> String -> List (Chunk String)
subChunks moduleDocs postDocs =
    subChunksHelp moduleDocs (String.split "," postDocs)


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
              []


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
        (Name.Canonical moduleDocs.name name, entry)

