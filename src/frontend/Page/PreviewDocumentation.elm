module Page.PreviewDocumentation where

import Html exposing (..)
import Dict

import Graphics.Element as Element
import Json.Decode as Json exposing ((:=))
--import Html.Attributes exposing (..)

import Component.Header as Header
import Component.PackageDocs as PDocs
import Component.PackageSidebar as PkgNav
import Docs.Package as Docs



dummySignal : Signal.Mailbox PDocs.Action
dummySignal =
  Signal.mailbox PDocs.NoOp


-- main : Html
main =
  --Element.show rawDocs
  PDocs.view dummySignal.address rawDocs



-- MODEL


type alias Model =
    { header : Header.Model
    , moduleDocs : PDocs.Model
    , pkgNav : PkgNav.Model
    }


readme : PDocs.Model
readme = PDocs.Readme ("# Elm Collision\n\nDetect collision/intersection of geometry in a defined coordinate space, AKA: tell me when objects are touching or overlapping\n\n![elm-collision demo](https://raw.githubusercontent.com/burabure/elm-collision/master/elm-collision.gif)\n\nThis library is useful for games, interactive apps, dynamic element composition and other cases where you need very efficient detection of overlapping objects\n\n\n### Get Started\n\n- Read the [the documentation][docs].\n- Try and read the code of [the examples][examples].\n\n[docs]: http://package.elm-lang.org/packages/BuraBure/elm-collision/latest/\n[examp…:\nhttp://github.com/burabure/elm-collision/tree/master/examples/\n\n\n### Contributing\n\nDo you have a suggestion, algorithm or formula that you'd like to add to this library?, I'd love to take a look at it and help you get it working with the library, just post an issue or send a pull request =D\n")


rawDocs =
  let
    json = Result.withDefault Dict.empty (Json.decodeString Docs.decodePackage doc)
    moduleName = "Collision2D"
  in
    case Dict.get moduleName json of
      Just moduleDocs ->
        let
          chunks =
            PDocs.toChunks moduleDocs
        in
          PDocs.RawDocs (PDocs.Info moduleName (PDocs.toNameDict json) chunks)

      Nothing ->
        PDocs.Loading


-- UPDATE



-- VIEW
{-
view : Signal.Address Action -> Model -> Html
view addr model =
  Header.view addr model.header
    [ PDocs.view (Signal.forwardTo addr UpdateDocs) (Debug.log "moduleDocs" model.moduleDocs)
    --, PkgNav.view (Signal.forwardTo addr UpdateNav) model.pkgNav
    ]
-}

doc : String
doc = """[
  {
    "name": "Collision2D",
    "comment": " Detect collision/intersection of geometry in a defined 2D coordinate space\\nAKA tell me when objects are touching or overlapping.\\n\\nAll objects use the same coordinate system you might see in an algebra or\\nphysics problem, origin (0,0) is at the center of the object,\\nso they're compatible with the core Graphics.Collage coordinate system.\\n\\n# Basic geometry\\n@docs Rectangle, rectangle, Circle, circle\\n\\n# Rectangle to Rectangle Collision\\n@docs axisAlignedBoundingBox, rectangleSide, Side\\n\\n# Circle to Circle Collision\\n@docs circleToCircle\\n",
    "aliases": [],
    "types": [
      {
        "name": "Circle",
        "comment": " Represents circular geometry.\\n",
        "args": [],
        "cases": []
      },
      {
        "name": "Rectangle",
        "comment": " Represents rectangular hitbox geometry.\\n",
        "args": [],
        "cases": []
      },
      {
        "name": "Side",
        "comment": " Represents sides of a Rectangle\\n",
        "args": [],
        "cases": [
          [
            "Top",
            []
          ],
          [
            "Right",
            []
          ],
          [
            "Bottom",
            []
          ],
          [
            "Left",
            []
          ]
        ]
      }
    ],
    "values": [
      {
        "name": "axisAlignedBoundingBox",
        "comment": " Super efficient collision detection between\\ntwo Rectangles that are axis aligned — meaning no rotation.\\n\\n    rect1 = rectangle 5 5 10 10\\n    rect2 = rectangle 7 5 10 10\\n\\n    axisAlignedBoundingBox rect1 rect2 -- True\\n    -- rect1 is coliding with rect2\\n",
        "type": "Collision2D.Rectangle -> Collision2D.Rectangle -> Bool"
      },
      {
        "name": "circle",
        "comment": " Create a Circle Hitbox from from coordinates (cx, cy) and geometry (radius)\\n\\nArguments:\\n\\n    circle centerX centerY radius\\n\\nExample:\\n\\n    circle 5 5 10 -- a radius 10 circle centered on coordinates 5,5\\n",
        "type": "Float -> Float -> Float -> Collision2D.Circle"
      },
      {
        "name": "circleToCircle",
        "comment": " Super efficient collision detection between two Circles\\n\\n    circle1 = circle 5 5 5\\n    circle2 = circle 7 5 5\\n\\n    circleToCircle circle1 circle2 -- True\\n    -- circle1 is coliding with circle2\\n",
        "type": "Collision2D.Circle -> Collision2D.Circle -> Bool"
      },
      {
        "name": "rectangle",
        "comment": " Create a Rectangle hitbox from coordinates (cx, cy) and geometry (width and height)\\n\\nArguments:\\n\\n    rectangle centerX centerY width height\\n\\nExample:\\n\\n    rectangle 5 5 10 10\\n    -- a 10 x 10 rectangle centered on coordinates 5,5\\n",
        "type": "Float -> Float -> Float -> Float -> Collision2D.Rectangle"
      },
      {
        "name": "rectangleSide",
        "comment": " Very efficiently detect which side of a Rectangle is colliding with another Rectangle\\n\\n    rect1 = rectangle 5 5 10 10\\n    rect2 = rectangle 7 5 10 10\\n\\n    rectangleSide rect1 rect2 -- Just Right\\n    -- rect1 is coliding with it's right side onto rect2\\n",
        "type": "Collision2D.Rectangle -> Collision2D.Rectangle -> Maybe.Maybe Collision2D.Side"
      }
    ],
    "generated-with-elm-version": "0.16.0"
  }
]"""
