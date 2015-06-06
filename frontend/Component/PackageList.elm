module Component.PackageList where

import Color
import ColorScheme as C
import Graphics.Element exposing  (..)
import Json.Decode exposing  (..)
import Set
import String
import Text


type alias Package =
    { name : String
    , summary : String
    , versions : List String
    }


package : Decoder Package
package =
    object3 Package
      ("name" := string)
      ("summary" := string)
      ("versions" := list string)


view : Int -> List String -> List Package -> Element
view innerWidth updatedPackages packages =
    let bigWords =
          Text.fromString "Packages"
            |> Text.height 30
            |> leftAligned

        packageListing =
          flow down <|
            case updatedPackages of
              [] ->
                  List.map (viewPackage innerWidth) packages

              _ ->
                  let
                    updatedPackagesSet =
                      Set.fromList updatedPackages


                    (new, old) =
                      List.partition (\{name} -> Set.member name updatedPackagesSet) packages

                    msg =
                      Text.fromString "Warning: packages below here are not yet updated for Elm 0.15!"
                        |> Text.color C.green
                        |> centered
                  in
                      [ flow down (List.map (viewPackage innerWidth) new)
                      , color C.lightGrey (spacer innerWidth 1)
                      , spacer innerWidth 50
                      , width innerWidth msg
                      , spacer innerWidth 50
                      , flow down (List.map (viewPackage innerWidth) old)
                      ]



    in
      flow down
        [ container innerWidth 100 midLeft bigWords
        , packageListing
        , spacer innerWidth 100
        ]


viewPackage : Int -> Package -> Element
viewPackage innerWidth package =
  let version =
        case List.head package.versions of
          Just v -> v

      pkgLink =
        Text.fromString package.name
          |> Text.link ("/packages/" ++ package.name ++ "/" ++ version)
          |> leftAligned

      summary =
        Text.fromString package.summary
          |> leftAligned
  in
      flow down
      [ color C.lightGrey (spacer innerWidth 1)
      , flow right
        [ container 300 36 midLeft pkgLink
        , container (innerWidth - 300) 36 midLeft summary
        ]
      ]