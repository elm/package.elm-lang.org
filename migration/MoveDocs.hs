{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module MoveDocs
  ( move
  )
  where


import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.ByteString.Lazy as BS
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.FilePath ((</>))

import qualified Crawl
import qualified Elm.Compiler.Module as Module
import qualified Elm.Compiler.Type as Type
import qualified Elm.Docs as Docs
import qualified Elm.Name as N
import qualified Elm.Package as Pkg
import qualified Task
import qualified Json.Encode as Encode



-- MOVE


move :: Pkg.Name -> Pkg.Version -> Task.Transaction ()
move pkg version =
  do  bits <- liftIO $ BS.readFile (Crawl.oldDir pkg version </> "documentation.json")
      case Json.eitherDecode bits of
        Left problem ->
          Task.bail $ "Problem parsing documentation.json:\n" ++ problem

        Right oldDocs ->
          let
            newDocs = List.map updateDocs oldDocs
          in
            liftIO $
              Encode.write (Crawl.newDir pkg version </> "docs.json") $
                Encode.list Docs.encode newDocs


updateDocs :: OldDocs -> Docs.Module
updateDocs (OldDocs (Txt moduleName) (Txt comment) aliases types values) =
  Docs.Module
    { Docs._name = moduleName
    , Docs._comment = comment
    , Docs._unions = Map.fromList (map toUnionPair types)
    , Docs._aliases = Map.fromList (map toAliasPair aliases)
    , Docs._values = Map.fromList (Maybe.mapMaybe toValuePair values)
    , Docs._binops = Map.fromList (Maybe.mapMaybe toBinopPair values)
    }


toUnionPair :: Union -> (N.Name, Docs.Union)
toUnionPair (Union (Txt name) (Txt comment) args cases) =
  ( name, Docs.Union comment args cases )


toAliasPair :: Alias -> (N.Name, Docs.Alias)
toAliasPair (Alias (Txt name) (Txt comment) args tipe) =
  ( name, Docs.Alias comment args tipe )


toValuePair :: Value -> Maybe (N.Name, Docs.Value)
toValuePair (Value (Txt name) (Txt comment) tipe _) =
  if Text.any isSymbol name then
    Nothing
  else
    Just (name, Docs.Value comment tipe)


toBinopPair :: Value -> Maybe (N.Name, Docs.Binop)
toBinopPair (Value (Txt name) (Txt comment) tipe fix) =
  if Text.any isSymbol name then
    Just
      ( name
      , case fix of
          Nothing ->
            Docs.Binop comment tipe Docs.Left (Docs.Precedence 9)

          Just (assoc, prec) ->
            Docs.Binop comment tipe assoc prec
      )
  else
    Nothing


isSymbol :: Char -> Bool
isSymbol char =
  Char.isSymbol char || Set.member char symbols


symbols :: Set.Set Char
symbols =
  Set.fromList "+-/*=.$<>:&|^?%#@~!"



-- OLD DOCS


newtype Txt =
  Txt Text.Text


data OldDocs =
  OldDocs
    { _moduleName :: Txt
    , _comment :: Txt
    , _aliases :: [Alias]
    , _types :: [Union]
    , _values :: [Value]
    }


data Alias =
  Alias
    { _aliasName :: Txt
    , _aliasComment :: Txt
    , _aliasArgs :: [N.Name]
    , _aliasType :: Type.Type
    }


data Union =
  Union
    { _unionName :: Txt
    , _unionComment :: Txt
    , _unionArgs :: [N.Name]
    , _unionCases :: [(N.Name, [Type.Type])]
    }


data Value =
  Value
    { _valueName :: Txt
    , _valueComment :: Txt
    , _valueType :: Type.Type
    , _valueFix :: Maybe (Docs.Associativity, Docs.Precedence)
    }



-- JSON for OLD DOCS


instance Json.FromJSON OldDocs where
    parseJSON (Json.Object obj) =
        OldDocs
            <$> obj .: "name"
            <*> obj .: "comment"
            <*> obj .: "aliases"
            <*> obj .: "types"
            <*> obj .: "values"

    parseJSON value =
        fail $ "Cannot decode OldDocs from: " ++ show (Json.encode value)


instance Json.FromJSON Alias where
    parseJSON (Json.Object obj) =
        Alias
            <$> obj .: "name"
            <*> obj .: "comment"
            <*> obj .: "args"
            <*> obj .: "type"

    parseJSON value =
        fail $ "Cannot decode Alias from: " ++ show (Json.encode value)


instance Json.FromJSON Union where
    parseJSON (Json.Object obj) =
        Union
            <$> obj .: "name"
            <*> obj .: "comment"
            <*> obj .: "args"
            <*> obj .: "cases"

    parseJSON value =
        fail $ "Cannot decode Union from: " ++ show (Json.encode value)


instance Json.FromJSON Value where
    parseJSON (Json.Object obj) =
        Value
            <$> obj .: "name"
            <*> obj .: "comment"
            <*> obj .: "type"
            <*> (liftM2 (,) <$> obj .:? "associativity" <*> obj .:? "precedence")

    parseJSON value =
        fail $ "Cannot decode Value from: " ++ show (Json.encode value)


instance Json.FromJSON Docs.Associativity where
  parseJSON value =
    case value of
      Json.String "left"  -> return Docs.Left
      Json.String "non"   -> return Docs.Non
      Json.String "right" -> return Docs.Right
      _ -> fail $ "Unknown precedence: " ++ show (Json.encode value)


instance Json.FromJSON Docs.Precedence where
  parseJSON value =
    Docs.Precedence <$> Json.parseJSON value


instance Json.FromJSON Txt where
  parseJSON (Json.String txt) =
    return $ Txt $
      Text.replace "\\u003c" "<" (Text.replace "\\u003e" ">" txt)

  parseJSON _ =
    fail "Need a STRING here."



-- JSON for TYPES


instance Json.FromJSON Type.Type where
  parseJSON value =
    let
      failure =
        fail $
          "Trying to decode a type string, but could not handle this value:\n"
          ++ show (Json.encode value)
    in
      case value of
        Json.String text ->
          maybe failure return (Type.fromText (Text.replace "\\u003e" ">" text))

        Json.Object obj ->
          fromObject obj

        _ ->
          failure


fromObject :: Json.Object -> Json.Parser Type.Type
fromObject obj =
  do  tag <- obj .: "tag"
      case (tag :: String) of
        "lambda" ->
            Type.Lambda
              <$> obj .: "in"
              <*> obj .: "out"

        "var" ->
            Type.Var
              <$> obj .: "name"

        "type" ->
            do  name <- obj .: "name"
                return $
                  if name == "_Tuple0" then Type.Unit else Type.Type name []

        "app" ->
            do  func <- obj .: "func"
                args <- obj .: "args"
                case func of
                  Type.Type name [] ->
                    case args of
                      (a:b:cs) | Text.isPrefixOf "_Tuple" name ->
                        return $ Type.Tuple a b cs

                      _ ->
                        return $ Type.Type name args

                  Type.Unit | null args ->
                    return Type.Unit

                  _ ->
                    fail $ "Error on " ++ show (Json.encode obj)

        "record" ->
            Type.Record
              <$> obj .: "fields"
              <*> obj .: "extension"

        _ ->
            fail $ "Error when decoding type with tag: " ++ tag
