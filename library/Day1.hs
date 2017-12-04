module Day1 where

import Control.Arrow
import Data.Char

captcha :: String -> Int
captcha =
  sum . fmap fst . filter (uncurry (==)) . uncurry zip . (id &&& drop 1 . cycle) . fmap digitToInt
