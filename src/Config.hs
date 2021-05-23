module Config
    ( width
    , height
    , zoom
    , textScale
    , maxX
    , minX
    , maxY
    , minY
    , fps
    , snekLength
    , snekSpeed
    , appleGrowth
    , speedUpFactor
    , powerUpTicks
    ) where

width, height, zoom, textScale :: Float
width = 800
height = 800
zoom = 16
textScale = 0.4

minX, maxX, minY, maxY :: Int
minX = negate maxX
maxX = floor $ width / 2 / zoom
minY = negate maxY
maxY = floor $ height / 2 / zoom

fps :: Int
fps = 60

snekLength :: Int
snekLength = 7

snekSpeed :: Float
snekSpeed = 16

appleGrowth, powerUpTicks :: Int
appleGrowth = 2
powerUpTicks = fps * 10

speedUpFactor :: Float
speedUpFactor = 1.25
