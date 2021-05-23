{-# LANGUAGE TupleSections #-}

module Game where

import           Control.Monad                  ( join )
import           Data.Bifunctor                 ( bimap )
import           System.Random                  ( getStdRandom
                                                , random
                                                , randomR
                                                )

import           Config

-- Types -----------------------------------------------------------------------

data Game = Game
    { powerUp  :: PowerUp
    , redSnek  :: Snek
    , blueSnek :: Snek
    , result   :: String
    }

data Snek = Snek
    { headPos    :: Pos
    , tailLength :: Int
    , tailSegs   :: [Pos]
    , velocity   :: Velocity
    , delta      :: [Velocity]
    , moveTimer  :: Float
    , affected   :: Maybe Effect
    }

type Pos = (Float, Float)

type Velocity = Pos

data SnekColor = Red | Blue

data Effect = Effect
    { effect   :: PowerUpType
    , duration :: Int
    }

data PowerUpType = Apple | Star | Orange
  deriving Eq

data PowerUp = PowerUp
    { puType :: PowerUpType
    , puPos  :: Pos
    }


-- Game ------------------------------------------------------------------------

moveSneks :: Float -> Game -> IO Game
moveSneks t g@(Game pu rs bs r)
    | null r    = pure $ Game pu (moveSnek t rs) (moveSnek t bs) r
    | otherwise = pure g

getSnek :: SnekColor -> Game -> Snek
getSnek Red  = redSnek
getSnek Blue = blueSnek

setSnek :: SnekColor -> Snek -> Game -> Game
setSnek Red  rs (Game pu _  bs r) = Game pu rs bs r
setSnek Blue bs (Game pu rs _  r) = Game pu rs bs r

setResult :: String -> Game -> Game
setResult r (Game pu rs bs _) = Game pu rs bs r

cutSnek :: SnekColor -> Pos -> Game -> Game
cutSnek c p g =
    let s  = getSnek c g
        ts = tailSegs s
    in  setSnek
            c
            (setSnekLength (min (length ts) (length $ takeWhile (/= p) ts)) s)
            g

setDelta :: SnekColor -> (Float, Float) -> Game -> IO Game
setDelta Red  v' (Game pu rs bs r) = pure $ Game pu (setSnekDelta v' rs) bs r
setDelta Blue v' (Game pu rs bs r) = pure $ Game pu rs (setSnekDelta v' bs) r

gameInit :: Game
gameInit = Game
    (PowerUp Apple (0, 0))
    (Snek (toPos (minX + 5, maxY - 5)) snekLength [] (0, -zoom) [] 0 Nothing)
    (Snek (toPos (maxX - 5, minY + 5)) snekLength [] (0, zoom) [] 0 Nothing)
    "[N]ew game, [Q]uit"

newGame :: Game
newGame = setResult "" gameInit

tick :: Game -> IO Game
tick g@(Game pu rs bs r) | null r = pure $ Game pu (tickSnek rs) (tickSnek bs) r
                         | otherwise = pure g

toPos :: (Int, Int) -> Pos
toPos (x, y) = (zoom * fromIntegral x, zoom * fromIntegral y)

-- Snek ------------------------------------------------------------------------

moveSnek :: Float -> Snek -> Snek
moveSnek dt s@(Snek h tl ts v d mt a) = if sp * t' >= 1
    then setSnekHeading $ Snek h' tl (take tl $ h : ts) v d (t' - 1 / sp) a
    else Snek h tl ts v d t' a
  where
    sp =
        if s `isAffectedBy` Orange then snekSpeed * speedUpFactor else snekSpeed
    t' = mt + dt
    h' = bimap (fst v +) (snd v +) h

growSnek :: Snek -> Snek
growSnek s = setSnekLength (tailLength s + appleGrowth) s

setSnekLength :: Int -> Snek -> Snek
setSnekLength tl (Snek h _ ts v d mt a) = Snek h tl ts v d mt a

applyPowerUp :: PowerUpType -> Snek -> Snek
applyPowerUp pu s@(Snek h tl ts v d mt _) = case pu of
    Apple -> growSnek s
    _     -> Snek h tl ts v d mt (Just $ Effect pu powerUpTicks)

setSnekDelta :: (Float, Float) -> Snek -> Snek
setSnekDelta v' s@(Snek h tl ts v d mt a)
    | v == join bimap negate v' = s
    | otherwise                 = Snek h tl ts v (d ++ [v']) mt a

setSnekHeading :: Snek -> Snek
setSnekHeading s@(Snek h tl ts v d mt a)
    | null d'   = s
    | otherwise = Snek h tl ts (head d) (tail d) mt a
    where d' = dropWhile (\x -> v == x || join bimap negate v == x) d

tickSnek :: Snek -> Snek
tickSnek (Snek h tl ts v d mt a) = Snek h tl ts v d mt (tickPowerUp a)

isAffectedBy :: Snek -> PowerUpType -> Bool
isAffectedBy s pu = case effect <$> affected s of
    Just x -> x == pu
    _      -> False

-- PowerUp ---------------------------------------------------------------------

randomPowerUp :: IO PowerUpType
randomPowerUp = do
    r <- getStdRandom random :: IO Float
    pure $ pu r
  where
    pu x | x < 0.10  = Star
         | x < 0.25  = Orange
         | otherwise = Apple

randomPos :: IO Pos
randomPos = do
    x <- getStdRandom $ randomR (negate (maxX - 2), maxX - 2)
    y <- getStdRandom $ randomR (negate (maxY - 2), maxY - 2)
    pure $ toPos (x, y)

newPowerUp :: Game -> IO Game
newPowerUp (Game _ rs bs r) = do
    pu  <- randomPowerUp
    pos <- randomPos
    pure $ Game (PowerUp pu pos) rs bs r

tickPowerUp :: Maybe Effect -> Maybe Effect
tickPowerUp (Just (Effect pu t)) =
    if t < 1 then Nothing else Just (Effect pu (t - 1))
tickPowerUp _ = Nothing

-- Collision -------------------------------------------------------------------

wallCollision :: Pos -> Bool
wallCollision (x, y) = abs x >= width / 2 || abs y >= height / 2

powerUpCollision :: Game -> IO Game
powerUpCollision g@(Game pu rs bs _)
    | headPos rs == puPos pu = newPowerUp
    $ setSnek Red (applyPowerUp (puType pu) rs) g
    | headPos bs == puPos pu = newPowerUp
    $ setSnek Blue (applyPowerUp (puType pu) bs) g
    | otherwise = pure g

headCollision :: Snek -> Snek -> (Bool, Bool)
headCollision rs bs
    | headPos rs /= headPos bs
    = (False, False)
    | otherwise
    = let srs = rs `isAffectedBy` Star
          sbs = bs `isAffectedBy` Star
      in  if srs == sbs then (True, True) else (not srs, not sbs)

tailCollision :: Game -> (Game, (Bool, Bool))
tailCollision g@(Game _ rs bs _) = case (rHit, bHit) of
    (Just rh, Just bh) -> case (srs, sbs) of
        (False, False) -> (g, (True, True))
        (True , False) -> (cutSnek rh rHead g, (False, True))
        (False, True ) -> (cutSnek bh bHead g, (True, False))
        (True, True) -> (cutSnek bh bHead $ cutSnek rh rHead g, (False, False))
    (Just rh, Nothing) ->
        if srs then (cutSnek rh rHead g, (False, False)) else (g, (True, False))
    (Nothing, Just bh) ->
        if sbs then (cutSnek bh bHead g, (False, False)) else (g, (False, True))
    (Nothing, Nothing) -> (g, (False, False))
  where
    srs   = rs `isAffectedBy` Star
    sbs   = bs `isAffectedBy` Star
    segs  = map (, Red) (tailSegs rs) ++ map (, Blue) (tailSegs bs)
    rHead = headPos rs
    rHit  = lookup rHead segs
    bHead = headPos bs
    bHit  = lookup bHead segs

checkCollisions :: Game -> IO Game
checkCollisions g@(Game _ rs bs r)
    | not $ null r = pure g
    | otherwise = case res of
        (True , True ) -> pure $ setResult "It's a tie!" tcg
        (True , False) -> pure $ setResult "Blue wins!" tcg
        (False, True ) -> pure $ setResult "Red wins!" tcg
        _              -> powerUpCollision tcg
  where
    wc        = (wallCollision $ headPos rs, wallCollision $ headPos bs)
    hc        = headCollision rs bs
    (tcg, tc) = tailCollision g
    res       = join bimap or $ unzip [wc, hc, tc]
