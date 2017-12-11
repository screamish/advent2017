module Day3 where

-- import Flow
-- import Data.Monoid
-- import Data.Semigroup
-- import Data.List (delete)

data Coord = Coord Int Int
data Vec = Vec Int Int

right, up, left, down :: Vec
right = Vec   1   0
up    = Vec   0   1
left  = Vec (-1)  0
down  = Vec   0 (-1)

origin :: Coord
origin = Coord 0 0

move :: Coord -> Vec -> Coord
move (Coord x y) (Vec dx dy) = Coord (x+dx) (y+dy)

positionsInSpiralOrder :: [Coord]
positionsInSpiralOrder =
  let increasingSeries = replicate 2 =<< [1..]
      directions = cycle [right, up, left, down]
      moves = do
        (count, direction) <- zip increasingSeries directions
        replicate count direction
  in scanl move origin moves

distance :: Int -> Int
distance n =
  let (Coord x y) = positionsInSpiralOrder !! (n - 1)
  in abs x + abs y
