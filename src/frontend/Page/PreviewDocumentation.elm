module Page.PreviewDocumentation where

import Html exposing (..)
import Html.Attributes exposing (..)

import Component.Header as Header
import Component.PackageDocs as PDocs
import Component.PackageSidebar as PkgNav


main =
  div [] [ text "hola" ]
  --view somesignaladdress model

-- MODEL


type alias Model =
    { header : Header.Model
    , moduleDocs : PDocs.Model
    , pkgNav : PkgNav.Model
    }


-- UPDATE


type Action
    = NoOp


-- VIEW
{-
view : Signal.Address Action -> Model -> Html
view addr model =
  Header.view addr model.header
    [ PDocs.view (Signal.forwardTo addr UpdateDocs) (Debug.log "moduleDocs" model.moduleDocs)
    --, PkgNav.view (Signal.forwardTo addr UpdateNav) model.pkgNav
    ]
-}
