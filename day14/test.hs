{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
import Data.Char
import Data.Functor
import Control.Applicative
import Control.Monad.State
import qualified Data.Set as S
import Control.Monad
import Control.Arrow
import Debug.Trace


pvpartial :: MonadState String m => m Int
pvpartial = state $ first read . break (not . isDigit)

skip = foldl (.) id . flip replicate tail

ppospartial :: MonadState String m => m (Int, Int)
ppospartial = do
    x <- pvpartial
    modify' tail
    y <- pvpartial
    return (x, y)

type Pos = (Int, Int)
type Geom = [Pos]

pgeom = evalState pgeom'
    where pgeom' = do x <- ppospartial
                      get >>= \case
                          [] -> return [x]
                          _ -> (x:) <$> (modify' (skip 4) >> pgeom')

genFromTo x y | x < y = [x..y]
              | otherwise = [y..x]

fillLine :: Pos -> Pos -> [Pos]
fillLine (fromx, fromy) (tox, toy) | tox == fromx = map (fromx,) (genFromTo fromy toy)
                                   | toy == fromy = map (,fromy) (genFromTo fromx tox)

fillGeom :: Geom -> [Pos]
fillGeom = concat . (zipWith fillLine <*> tail)

fillGeoms :: [Geom] -> [Pos]
fillGeoms = concatMap fillGeom


data BB = BB { minX :: Int, minY :: Int, maxX :: Int, maxY :: Int }
    deriving Show

getBB :: [Pos] -> BB
getBB = foldr updateBB (BB maxBound maxBound 0 0)
    where updateBB pos = upMinX pos . upMinY pos . upMaxY pos . upMaxX pos
          upMinX (x,_) bb = bb { minX = min x (minX bb) }
          upMinY (_,y) bb = bb { minY = min y (minY bb) }
          upMaxX (x,_) bb = bb { maxX = max x (maxX bb) }
          upMaxY (_,y) bb = bb { maxY = max y (maxY bb) }

bbspan :: BB -> (Int, Int)
bbspan (BB mnx mny mxx mxy) = (mxx - mnx, mxy - mny)

fetchBB x = getBB . fillGeoms .  map pgeom . lines <$> readFile x

input = lines <$> readFile "input.txt"

geoms = map pgeom <$> input

