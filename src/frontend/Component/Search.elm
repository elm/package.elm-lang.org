module Component.Search (..) where

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
import Component.PackageDocs as PDocs
import Docs.Summary as Summary
import Docs.Entry as Entry
import Docs.Name as Name
import Docs.Package as Docs
import Docs.Type as Type
import Docs.Version as Version
import Page.Context as Ctx
import Parse.Type as Type
import Utils.Path exposing ((</>))


-- MODEL


type Model
    = Loading
    | Failed Http.Error
    | Catalog (List Summary.Summary)
    | Docs Info


type alias Info =
    { packageDict : Packages
    , chunks : List Chunk
    , failed : List Summary.Summary
    , query : String
    , excludedPackages : Set.Set PackageIdentifier
    , focusedPackage : Maybe PackageIdentifier
    }


type alias PackageIdentifier =
    String


type alias Packages =
    Dict.Dict PackageIdentifier PackageInfo


type alias PackageInfo =
    { package : Docs.Package
    , context : Ctx.VersionContext
    , nameDict : Name.Dictionary
    }


type alias Chunk =
    { package : PackageIdentifier
    , name : Name.Canonical
    , entry : Entry.Model Type.Type
    , entryNormalized : Entry.Model Type.Type
    }



-- INIT


init : ( Model, Effects Action )
init =
    ( Loading
    , getPackageInfo
    )



-- UPDATE


type Action
    = Fail Http.Error
    | Load ( List Summary.Summary, List String )
    | FailDocs Summary.Summary
    | LoadDocs Ctx.VersionContext Docs.Package
    | Query String
    | ToggleFocus PackageIdentifier


update : Action -> Model -> ( Model, Effects Action )
update action model =
    case action of
        Query query ->
            flip (,) Fx.none
                <| case model of
                    Docs info ->
                        Docs { info | query = query }

                    _ ->
                        model

        Fail httpError ->
            ( Failed httpError
            , Fx.none
            )

        Load ( allSummaries, updatedPkgs ) ->
            let
                updatedSet =
                    Set.fromList updatedPkgs

                ( summaries, oldSummaries ) =
                    List.partition (\{ name } -> Set.member name updatedSet) allSummaries

                contextEffects = List.map getDocs summaries
            in
                ( Catalog summaries
                , Fx.batch contextEffects
                )

        FailDocs summary ->
            case model of
                Docs info ->
                    ( Docs { info | failed = summary :: info.failed }
                    , Fx.none
                    )

                _ ->
                    ( Docs (Info (Dict.empty) [] [ summary ] "" Set.empty Nothing)
                    , Fx.none
                    )

        LoadDocs ctx docs ->
            let
                { user, project, version } = ctx

                pkgName = user </> project </> version

                pkgInfo = PackageInfo docs ctx (PDocs.toNameDict docs)

                chunks =
                    docs
                        |> Dict.toList
                        |> List.concatMap (\( name, moduleDocs ) -> toChunks pkgName moduleDocs)
            in
                case model of
                    Docs info ->
                        ( Docs
                            { info
                                | packageDict = Dict.insert pkgName pkgInfo info.packageDict
                                , chunks = List.append info.chunks chunks
                            }
                        , Fx.none
                        )

                    _ ->
                        ( Docs (Info (Dict.singleton pkgName pkgInfo) chunks [] "" Set.empty Nothing)
                        , Fx.none
                        )

        ToggleFocus package ->
            case model of
                Docs info ->
                    let
                      newFocusedPackage =
                        case info.focusedPackage of
                          Just currentPackage ->
                            if currentPackage == package then
                              Nothing
                            else
                              Just package
                          Nothing ->
                            Just package
                    in
                      ( Docs { info | focusedPackage = newFocusedPackage }
                      , Fx.none
                      )

                _ ->
                    ( model
                    , Fx.none
                    )


latestVersionContext : Summary.Summary -> Result String Ctx.VersionContext
latestVersionContext summary =
    let
        userProjectList =
          List.take 2 (String.split "/" summary.name)

        latestVersionSingleton =
          summary.versions
            |> List.take 1
            |> List.map Version.vsnToString

    in
      case List.append userProjectList latestVersionSingleton of
        [user, project, version] ->
          Result.Ok
            (Ctx.VersionContext user project version [] Nothing)

        _ ->
          Result.Err
            "Summary is corrupted"


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
            |> Task.map Load
            |> flip Task.onError (Task.succeed << Fail)
            |> Fx.task


getDocs : Summary.Summary -> Effects Action
getDocs summary =
    let
        contextResult = latestVersionContext summary

        failTask = Task.succeed (FailDocs summary)
    in
      case contextResult of
        Result.Ok context ->
          Ctx.getDocs context
              |> Task.map (LoadDocs context)
              |> (flip Task.onError) (always failTask)
              |> Fx.task

        Result.Err error ->
          Fx.task failTask



-- VIEW


view : Signal.Address Action -> Model -> Html
view addr model =
    div [ class "search" ]
        <| case model of
            Loading ->
                [ p [] [ text "Loading list of packages..." ]
                ]

            Failed httpError ->
                [ p [] [ text "Package summary did not load." ]
                , p [] [ text (toString httpError) ]
                ]

            Catalog catalog ->
                [ p [] [ text <| "Loading docs for " ++ toString (List.length catalog) ++ "packages..." ]
                ]

            Docs info ->
                [ input
                  [ placeholder "Search function by name or type"
                  , value info.query
                  , on "input" targetValue (Signal.message addr << Query)
                  ]
                  []
                , div [] (viewSearchResults addr info)
                , h3 [] [ text "Could not load or parse documentation of the following packages" ]
                , ul
                    []
                    (List.map (\summary -> li [] [ text summary.name] ) info.failed)
                ]


viewSearchResults : Signal.Address Action -> Info -> List Html
viewSearchResults addr ({ query, chunks } as info) =
    let
        queryType = Type.normalize (PDocs.stringToType query)
    in
        if String.isEmpty query then
            searchIntro addr
        else
            let
                filteredChunks =
                    case queryType of
                        Type.Var string ->
                            chunks
                                |> List.map (\chunk -> ( Entry.nameDistance query chunk.entry, chunk ))
                                |> List.filter (\( distance, _ ) -> distance < 10)

                        _ ->
                            chunks
                                |> List.map (\chunk -> ( Entry.typeDistance queryType chunk.entryNormalized, chunk ))
                                |> List.filter (\( distance, _ ) -> distance < 10)

                filteredChunksPackages =
                    filteredChunks
                        |> List.foldl
                            (\( _, chunk ) -> Set.insert chunk.package)
                            Set.empty
                        |> Set.toList
            in
                searchResultsChunks info filteredChunks
                -- Disable package filter for now as it needs mor thought, e.g. what happens when the currently focused pagckage is not in new search results. It also needs a nice UI.
                --[ div [] (searchResultsPackages addr filteredChunksPackages)
                --, div [] (searchResultsChunks info filteredChunks)
                --]


searchResultsPackages : Signal.Address Action -> List PackageIdentifier -> List Html
searchResultsPackages addr packages =
    List.map
      (\ identifier -> button [ onClick addr (ToggleFocus identifier) ] [ text identifier ])
      packages


searchResultsChunks : Info -> List (Int, Chunk) -> List Html
searchResultsChunks {packageDict, focusedPackage} weightedChunks =
    weightedChunks
        |> List.sortBy (\( distance, _ ) -> distance)
        |> List.filter (\( _, {package} ) -> focusedPackage == Nothing || focusedPackage == Just package)
        |> List.map (\( _, { package, name, entry } ) -> Entry.typeViewAnnotation package name (nameDict packageDict package) entry)


searchIntro : Signal.Address Action -> List Html
searchIntro addr =
    [ h1 [] [ text "Welcome to the Elm API Search" ]
    , p [] [ text "Search the modules of the latest Elm packages by either function name or by approximate type signature." ]
    , h2 [] [ text "Example searches" ]
    , ul
        []
        (List.map
            (\query ->
                li [] [ a [ href "#", onClick addr (Query query) ] [ text query ] ]
            )
            exampleSearches
        )
    ]


exampleSearches : List String
exampleSearches =
    [ "map"
    , "(a -> b -> b) -> b -> List a -> b"
    , "(a -> b -> c) -> b -> a -> c"
    , "Result x a -> (a -> Result x b) -> Result x b"
    ]



-- MAKE CHUNKS


toChunks : PackageIdentifier -> Docs.Module -> List Chunk
toChunks pkgIdent moduleDocs =
    case String.split "\n@docs " moduleDocs.comment of
        [] ->
            []

        firstChunk :: rest ->
            List.concatMap (subChunks pkgIdent moduleDocs) rest


subChunks : PackageIdentifier -> Docs.Module -> String -> List Chunk
subChunks pkgIdent moduleDocs postDocs =
    catMaybes (subChunksHelp pkgIdent moduleDocs (String.split "," postDocs))


subChunksHelp : PackageIdentifier -> Docs.Module -> List String -> List (Maybe Chunk)
subChunksHelp pkgIdent moduleDocs parts =
    case parts of
        [] ->
            []

        rawPart :: remainingParts ->
            let
                part =
                    String.trim rawPart
            in
                case PDocs.isValue part of
                    Just valueName ->
                        toMaybeChunk pkgIdent moduleDocs valueName
                            :: subChunksHelp pkgIdent moduleDocs remainingParts

                    Nothing ->
                        let
                            trimmedPart =
                                String.trimLeft rawPart
                        in
                            case String.words trimmedPart of
                                [] ->
                                    []

                                token :: _ ->
                                    case PDocs.isValue token of
                                        Just valueName ->
                                            [ toMaybeChunk pkgIdent moduleDocs valueName ]

                                        Nothing ->
                                            []


toMaybeChunk : PackageIdentifier -> Docs.Module -> String -> Maybe Chunk
toMaybeChunk pkgIdent moduleDocs name =
    case Dict.get name moduleDocs.entries of
        Nothing ->
            Nothing

        Just e ->
            let
                entry = Entry.map PDocs.stringToType e

                entryNormalized = Entry.map Type.normalize entry
            in
              Just <|
                Chunk
                    pkgIdent
                    (Name.Canonical moduleDocs.name name)
                    entry
                    entryNormalized


nameDict : Packages -> PackageIdentifier -> Name.Dictionary
nameDict packageDict name =
    case Dict.get name packageDict of
        Just info ->
            .nameDict info

        Nothing ->
            Dict.empty


chunkPackage : Packages -> PackageIdentifier -> Docs.Package
chunkPackage packageDict name =
    case Dict.get name packageDict of
        Just info ->
            .package info

        Nothing ->
            Dict.empty


catMaybes : List (Maybe a) -> List a
catMaybes xs =
  case xs of
    [] -> []
    (Nothing::xs') -> catMaybes xs'
    (Just x::xs') -> x :: catMaybes xs'
