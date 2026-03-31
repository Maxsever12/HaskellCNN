module Pooling where 
import Types

maxPool2 :: Channel -> Channel
maxPool2 channel = Channel
  { height = height channel `div` 2
  , width = width channel `div` 2
  , pixels = poolPixels (pixels channel)
  }

poolPixels :: [[Float]] -> [[Float]]
poolPixels [] = []
poolPixels (row1:row2:rows) = poolRows row1 row2 : poolPixels rows
poolPixels _ = [] -- Handle odd number of rows

poolRows :: [Float] -> [Float] -> [Float]
poolRows (x1:x2:xs) (y1:y2:ys) = max (max x1 x2) (max y1 y2) : poolRows xs ys
poolRows _ _ = [] -- Handle odd number of columns