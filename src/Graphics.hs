module Graphics where

import           Config

import           Game                           ( Effect(..)
                                                , PowerUp(..)
                                                , PowerUpType(..)
                                                , Snek
                                                , SnekColor(..)
                                                , affected
                                                , toPos
                                                )

import           Graphics.Gloss

backgroundColor :: Color
backgroundColor = light black

headShape, segmentShape :: Picture
headShape = rectangleSolid zoom zoom
segmentShape = rectangleSolid zoom zoom

groundPic, applePic, starPic, speedUpPic :: Picture
groundPic = uncurry rectangleSolid $ toPos (maxX * 2 - 1, maxY * 2 - 1)
applePic = color green $ circleSolid $ 0.42 * zoom
starPic = color (bright yellow) $ scale (zoom / 2) (zoom / 2) starLine
    where
        starLine = line
                [ coords !! 0
                , coords !! 2
                , coords !! 4
                , coords !! 1
                , coords !! 3
                , coords !! 0
                ]
        coords = map
                (\x -> (cos (2 * pi * x / 360), sin (2 * pi * x / 360)))
                [18, 90 .. 360]
speedUpPic = color (bright orange) $ circleSolid $ 0.42 * zoom

baseColor :: SnekColor -> Color
baseColor Red  = red
baseColor Blue = blue

snekTailColor :: SnekColor -> Snek -> Color
snekTailColor c _ = dim $ baseColor c

snekHeadColor :: SnekColor -> Snek -> Color
snekHeadColor c s = case affected s of
        Just (Effect Star _) -> mixColors 0.2 0.8 (baseColor c) (bright yellow)
        Just (Effect Orange _) ->
                mixColors 0.2 0.8 (baseColor c) (bright orange)
        _ -> baseColor c

powerUpPic :: PowerUp -> Picture
powerUpPic (PowerUp Apple  _) = applePic
powerUpPic (PowerUp Star   _) = starPic
powerUpPic (PowerUp Orange _) = speedUpPic
