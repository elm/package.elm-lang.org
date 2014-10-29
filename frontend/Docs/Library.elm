module Website.Docs.Library (docs,scene) where

import Dict
import Http
import Json
import JavaScript.Experimental as JSE
import Maybe
import String

-- Extract library info from JSON

docs : String -> String -> Signal Doc
docs name version =
    let path = "/catalog/" ++ name ++ "/" ++ version ++ "/docs.json"
    in  extract <~ Http.sendGet (constant path)

extract : Http.Response String -> Doc
extract response =
    case response of
      Http.Success str ->
          case Json.fromString str of
            Just (Json.Array xs) ->
                let rawModules = map (JSE.toRecord . JSE.fromJson) xs
                in  Doc (map .name rawModules) (concatMap toValues rawModules)
            _ -> Doc [] []
      _ -> Doc [] []

type Value = { name:String, home:String }
type Doc = { modules : [String], values : [Value] }

toValues modul =
    let home = modul.name
        toValue v = Value v.name home
    in
        concat [ map toValue modul.values
               , map toValue modul.datatypes
               , map toValue modul.aliases
               ]

-- Search for library info

search : Doc -> String -> Dict.Dict String [String]
search doc term =
    case term of
      "" -> Dict.fromList <| map (\m -> (m,[])) doc.modules
      _  -> let dict = foldl (\m d -> Dict.insert m [] d) Dict.empty <|
                       filter (contains term) doc.modules
                insert value dict =
                    Dict.update value.home (updater value.name) dict
            in  
                foldr insert dict <| filter (contains term . .name) doc.values

contains : String -> String -> Bool
contains a b =
    String.contains (String.toLower a) (String.toLower b)

updater : a -> Maybe [a] -> Maybe [a]
updater x maybe =
    Just (x :: Maybe.maybe [] id maybe)

-- Show the search results

scene : String -> String -> String -> Doc -> Element
scene project version term doc =
    let path = "/catalog/" ++ project ++ "/" ++ version ++ "/"
        results = flow down . map (showModule path) . Dict.toList <| search doc term
    in  spacer 20 4 `beside` results

showModule : String -> (String, [String]) -> Element
showModule path (name, values) =
    let results = flow down <| map (valueLink path name) values
    in
      flow down [ moduleLink path name
                , results
                , spacer 10 (if isEmpty values then 6 else 16) ]

moduleLink : String -> String -> Element
moduleLink path name =
    leftAligned . Text.link (path ++ toPath name) <| toText name

valueLink : String -> String -> String -> Element
valueLink path modul name =
    let address = path ++ toPath modul ++ "#" ++ name
    in  flow down
        [ spacer 10 4
        , spacer 20 4 `beside` (leftAligned . monospace . Text.link address <| toText name)
        ]

toPath : String -> String
toPath = String.map (\c -> if c == '.' then '-' else c)

