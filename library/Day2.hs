module Day2 where

import Control.Arrow
import Flow
import Data.Monoid
import Data.Semigroup
import Data.List (delete)

checksum :: String -> Int
checksum =
  let lineChecksum =
        words
        .> fmap read
        .> foldMap (Min &&& Max)
        .> (getMin *** getMax)
        .> uncurry subtract
  in lines .> fmap lineChecksum .> foldMap Sum .> getSum

checksum2 :: String -> Int
checksum2 =
  let firstDivisors :: [Int] -> Int
      firstDivisors xs =
        head [x `quot` y | x <- xs, y <- delete x xs, x `rem` y == 0]
      lineChecksum =
        words
        .> fmap read
        .> firstDivisors
  in lines .> fmap lineChecksum .> sum
