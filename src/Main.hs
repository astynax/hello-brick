module Main where

import Brick
import Brick.Widgets.Border (borderWithLabel, borderAttr)
import Control.Monad (void)
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

type State = Int
type Name = Int

main :: IO ()
main = void $ defaultMain app 0

app :: App State Event Name
app = App
  { appDraw         = draw
  , appChooseCursor = const . const $ Nothing
  , appHandleEvent  = handle
  , appStartEvent   = return
  , appAttrMap      = const . setBorderColor blue $ attrMap defAttr []
  }

draw :: State -> [Widget Name]
draw s =
  [ (i 0 <+> (i 1 <=> (((i 4 <+> (i 5 <=> i 6)) <=> i 3) <+> i 2)))
    <=>
    str "[q] - quit, [tab] - switch frame, <arrows> - scroll current frame"
  ]
  where
    i idx =
      decorate
      . borderWithLabel (str $ "[ " ++ show idx ++ " ]")
      $ loremVP idx
      where
        decorate
          | s == idx  = updateAttrMap $ setBorderColor yellow
          | otherwise = id

handle :: State -> BrickEvent Name Event -> EventM Name (Next State)
handle s (VtyEvent (EvKey k [])) =
  case k of
    KChar 'q'  ->
      halt s
    KChar '\t' ->
      continue $ (s + 1) `mod` 7
    KUp        ->
      do' vScrollBy (-1)
    KDown      ->
      do' vScrollBy 1
    KLeft      ->
      do' hScrollBy (-1)
    KRight     ->
      do' hScrollBy 1
    _          ->
      continue s
  where
    do' f x = f (viewportScroll s) x >> continue s
handle s _ = continue s

setBorderColor :: Color -> AttrMap -> AttrMap
setBorderColor c =
  applyAttrMappings [(borderAttr, defAttr `withForeColor` c)]

loremVP :: Int -> Widget Int
loremVP idx =
  viewport idx Both . str $ lorem

lorem :: String
lorem = unlines
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
