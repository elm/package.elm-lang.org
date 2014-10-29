module Catalog where

import String
import Http
import Json
import JavaScript.Experimental as JSE
import Text
import Website.ColorScheme as C
import Website.Skeleton (skeleton)
import Window

jsonResponse = Http.sendGet (constant "/libraries.json")

addSearchText library =
    { library | searchText = String.toLower <| library.name ++ " " ++ library.summary }

format : Http.Response String -> [Library]
format response =
    case response of
      Http.Success str ->
          case Json.fromString str of
            Just (Json.Array xs) ->
                map (addSearchText << JSE.toRecord << JSE.fromJson) xs
            _ -> []
      _ -> []

type Library = { name:String, summary:String, searchText:String, versions:[String] }

search : String -> [Library] -> [Library]
search term libraries =
    let lowTerm = String.toLower term in
    if String.length term < 2 then libraries else
        filter (String.contains lowTerm << .searchText) libraries

libraries : Signal [Library]
libraries = format <~ jsonResponse

deslash = String.map (\c -> if c == '/' then '-' else c)

row : Int -> Library -> Element
row w library =
    let url = "/catalog/" ++ deslash library.name ++ "/" ++ head library.versions in
    flow down
    [ color C.mediumGrey <| spacer w 1
    , flow right [ container 300 36 midLeft (leftAligned << Text.link url <| toText library.name)
                 , container (w-300) 36 midLeft (plainText library.summary)
                 ]
    ]

scene term libraries w =
    flow down <| map (row w) (search term libraries)

main = skeleton "filter" [] scene libraries
