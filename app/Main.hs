module Main where

import           Control.Monad                  ( join )
import           System.Exit                    ( exitSuccess )

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game

import           Config
import           Game
import           Graphics

window :: Display
window = InWindow "Angry Sneks" (round width, round height) (20, 20)

handleKeys :: Event -> Game -> IO Game
handleKeys (EventKey (Char 'q') Down _ _) _ = do
    putStrLn "Goodbye!"
    exitSuccess
handleKeys (EventKey (Char 'n') Down _ _) _ = pure newGame
handleKeys (EventKey (Char 'p') Down _ _) g
    | null $ result g       = pure $ setResult "Paused." g
    | result g == "Paused." = pure $ setResult "" g
    | otherwise             = pure g
handleKeys (EventKey (Char       'w'  ) Down _ _) g = setDelta Red (0, zoom) g
handleKeys (EventKey (Char       'a'  ) Down _ _) g = setDelta Red (-zoom, 0) g
handleKeys (EventKey (Char       's'  ) Down _ _) g = setDelta Red (0, -zoom) g
handleKeys (EventKey (Char       'd'  ) Down _ _) g = setDelta Red (zoom, 0) g
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) g = setDelta Blue (0, zoom) g
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) g =
    setDelta Blue (-zoom, 0) g
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) g =
    setDelta Blue (0, -zoom) g
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) g =
    setDelta Blue (zoom, 0) g
handleKeys _ g = pure g

update :: Float -> Game -> IO Game
update t g = checkCollisions =<< tick =<< moveSneks t g

render :: Game -> IO Picture
render g =
    pure
        $  pictures
        $  (if not $ null res then (++ [resPic]) else id)
        $  groundPic
        :  puPic
        :  snekPic Red  (redSnek g)
        ++ snekPic Blue (blueSnek g)
  where
    res = result g
    resPic =
        uncurry translate (toPos (minX + 3, minY + 3))
            $ join scale textScale
            $ color white
            $ text res
    pu    = powerUp g
    puPic = uncurry translate (puPos pu) (powerUpPic pu)
    snekPic c s =
        uncurry translate (headPos s) (color (snekHeadColor c s) headShape)
            : map
                  (\pos -> uncurry translate
                                   pos
                                   (color (snekTailColor c s) segmentShape)
                  )
                  (tailSegs s)

main :: IO ()
main = playIO window backgroundColor fps gameInit render handleKeys update
