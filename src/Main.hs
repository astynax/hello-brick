module Main where

import Brick.AttrMap (applyAttrMappings)
import Brick.Main (App(..), defaultMain, halt, continue,
                   viewportScroll, ViewportScroll(hScrollBy, vScrollBy))
import Brick.Types (ViewportType(Both))
import Brick.Widgets.Border (borderWithLabel, borderAttr)
import Brick.Widgets.Border.Style (borderStyleFromChar)
import Brick.Widgets.Core (str, (<=>), (<+>), updateAttrMap, viewport)
import Control.Monad (void)
import Data.Default (def)
import Graphics.Vty.Attributes (withForeColor, blue, yellow)
import Graphics.Vty.Input.Events (Event(EvKey), Key(..))

type State = Int
type Name = Int

app :: App State Event Name
app =
    App
    { appDraw = draw
    , appChooseCursor = const . const $ Nothing
    , appHandleEvent = handle
    , appStartEvent = return
    , appAttrMap = const . setBorderColor blue $ def
    , appLiftVtyEvent = id
    }

main :: IO ()
main = void $ defaultMain app 0

draw s =
    [ (i 0 <+> (i 1 <=> (((i 4 <+> (i 5 <=> i 6)) <=> i 3) <+> i 2)))
      <=>
      str "[q] - quit, [tab] - switch frame, <arrows> - scroll current frame"
    ]
  where
    i idx =
        let decorate = if s == idx
                 then updateAttrMap $ setBorderColor yellow
                 else id
        in decorate
           . borderWithLabel (str $ "[ " ++ show idx ++ " ]")
           $ loremVP idx

handle s (EvKey k []) =
    case k of
        KChar 'q' ->
            halt s
        KChar '\t' ->
            continue $ (s + 1) `mod` 7
        KUp ->
            do' vScrollBy (-1)
        KDown ->
            do' vScrollBy 1
        KLeft ->
            do' hScrollBy (-1)
        KRight ->
            do' hScrollBy 1
        _ ->
            continue s
  where
    do' f x = f (viewportScroll s) x >> continue s

handle s _ = continue s

setBorderColor c =
    applyAttrMappings [(borderAttr, def `withForeColor` c)]

loremVP idx =
    viewport idx Both . str $ lorem

lorem :: String
lorem =
    unlines
    [ "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
    , "Duis ullamcorper, mi at laoreet auctor, est leo cursus ex,"
    , "ac gravida ipsum felis sit amet nunc. In in egestas ipsum,"
    , "ut pretium dui. Fusce sollicitudin volutpat lorem, eu"
    , "imperdiet tellus efficitur nec. Aenean sapien orci,"
    , "commodo ullamcorper euismod quis, rutrum et velit."
    , "Nulla pulvinar pharetra dui, in malesuada est aliquet"
    , "vitae. Nullam a efficitur nisi. Nullam libero tellus,"
    , "porta id ipsum et, placerat rutrum eros. Sed a cursus risus."
    , "Nulla vestibulum, lectus eget commodo aliquam, lectus erat"
    , "mollis diam, et rhoncus nisl eros sit amet metus."
    , "Etiam posuere posuere commodo. Morbi commodo et lectus"
    , "non cursus. Duis mauris lectus, sagittis ut mattis vitae,"
    , "tincidunt ac enim. Quisque id elementum lacus, nec dapibus"
    , "diam. Sed sagittis elit dignissim venenatis porta."
    ]
