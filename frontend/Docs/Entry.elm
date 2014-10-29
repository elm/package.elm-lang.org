module Website.Docs.Entry (entry) where

import String
import Website.ColorScheme as C

entry : Int -> String -> String -> Maybe (String,Int) -> Element -> Element
entry w name tipe assocPrec prose =
    let box n pos txt = container w (heightOf txt + n) pos txt
        ap = case assocPrec of
               Nothing -> []
               Just (a,p) -> [ box 2 topRight . leftAligned . Text.height 12 . toText <|
                                   a ++ "-associative, precedence " ++ show p ++ " " ]
        tipe' = box 2 topLeft . leftAligned <| monospace (toText " ") ++ prettify tipe
    in
      flow down [ tag name . color C.mediumGrey <| spacer w 1
                , color (rgb 238 238 240) <| layers <| ap ++ [ tipe' ]
                , flow right [ spacer 40 10, width (w-40) prose ]
                , spacer w 12
                ]

until c xs =
  case String.uncons xs of
    Nothing -> ("","")
    Just (hd,tl) ->
        if | hd == '(' -> let (chomped,rest) = until ')' tl
                              (before,after) = until c rest
                          in  (String.cons hd chomped ++ before, after)
           | hd == c -> ("", xs)
           | otherwise -> let (before,after) = until c tl
                          in  (String.cons hd before, after)

prettify raw =
    if | String.startsWith "type " raw || String.startsWith "data " raw ->
           let (name, rest) = until ' ' (String.slice 5 (String.length raw) raw) in
           monospace <| concat [ Text.color C.accent1 <| toText (String.slice 0 5 raw)
                               , bold <| toText name
                               , colorize "" rest
                               ]
       | otherwise ->
           let (name, rest) = until ':' raw
           in  monospace <| bold (toText name) ++ colorize "" rest

colorize stuff str =
  let continue clr op rest =
          toText (String.reverse stuff) ++ Text.color clr (toText op) ++ colorize "" rest
  in 
  case String.uncons str of
    Nothing -> toText (String.reverse stuff)
    Just (c,rest) ->
        if | c == ':' -> continue C.accent1 ":" rest
           | c == '|' -> continue C.accent1 "|" rest
           | c == '=' -> continue C.accent1 "=" rest
           | c == '-' -> case String.uncons rest of
                           Just ('>',rest') -> continue C.accent1 "->" rest'
                           _ -> colorize (String.cons c stuff) rest
           | otherwise -> colorize (String.cons c stuff) rest
