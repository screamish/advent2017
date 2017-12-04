module Day1 where

import Control.Arrow
import Data.Char
import Flow

captchaWithShift :: Int -> String -> Int
captchaWithShift n =
  fmap digitToInt
  .> (id &&& cycle .> drop n)
  .> uncurry zip
  .> filter (uncurry (==))
  .> fmap fst
  .> sum

captcha :: String -> Int
captcha = captchaWithShift 1

captcha2 :: String -> Int
captcha2 input =
  let shift = length input `div` 2
  in captchaWithShift shift input
