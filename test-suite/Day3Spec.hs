module Day3Spec where

import Test.Tasty.Hspec
import Data.String.Here.Uninterpolated

import qualified Day3

spec_day3 :: Spec
spec_day3 = parallel $ do
  --- Day 3: Spiral Memory ---

  -- You come across an experimental new kind of memory stored on an infinite two-dimensional grid.

  -- Each square on the grid is allocated in a spiral pattern starting at a location marked 1 and then counting up while spiraling outward. For example, the first few squares are allocated like this:

  -- 17  16  15  14  13
  -- 18   5   4   3  12
  -- 19   6   1   2  11
  -- 20   7   8   9  10
  -- 21  22  23---> ...

  -- While this is very space-efficient (no squares are skipped), requested data must be carried back to square 1 (the location of the only access port for this memory system) by programs that can only move up, down, left, or right. They always take the shortest path: the Manhattan Distance between the location of the data and square 1.

  -- Your puzzle input is 361527.
  let input = 361527

  it "Data from square 1 is carried 0 steps, since it's at the access port." $ do
    Day3.distance 1 `shouldBe` 0
  it "Data from square 12 is carried 3 steps, such as: down, left, left." $ do
    Day3.distance 12 `shouldBe` 3
  it "Data from square 23 is carried only 2 steps: up twice." $ do
    Day3.distance 23 `shouldBe` 2
  it "Data from square 1024 must be carried 31 steps." $ do
    Day3.distance 1024 `shouldBe` 31

  it "solves part 1" $ do
    -- How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?
    Day3.distance input `shouldBe` 326
  -- describe "part 2" $ do
