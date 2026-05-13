module Main where

import Numeric.LinearAlgebra
import Data.List (sum)
import System.Random

getPatch :: Matrix Double -> (Int, Int) -> (Int, Int) -> Matrix Double
getPatch input (x, y) (filterWidth, filterHeight) = subMatrix (x, y) (filterWidth, filterHeight) input

dotProduct :: Matrix Double -> Matrix Double -> Double
dotProduct a b = sumElements (a * b)

-- Convolution with an input and a kernel. Stride is the step size for moving the kernel across the input.
convolve :: Matrix Double -> Matrix Double -> Int -> Matrix Double
convolve input kernel stride = 
    let (imH, imW) = (rows input, cols input)
        (kH, kW)   = (rows kernel, cols kernel)
        outH       = (imH - kH) + stride
        outW       = (imW - kW) + stride
    in build (outH, outW) (\r c ->
        let patch = getPatch input (round r * stride, round c * stride) (kH, kW)
        in dotProduct patch kernel)

-- | Element-wise ReLU Activation function
relu :: Matrix Double -> Matrix Double
relu = cmap $ max 0

-- | Max Pooling
maxPool2x2 :: Matrix Double -> Int -> Matrix Double
maxPool2x2 img size =
    let outH = rows img `div` size
        outW = cols img `div` size
    in build (outH, outW) $ \r c ->
        let patch = getPatch img (size * round r, size * round c) (size, size)
        in maxElement patch

randomList :: Int -> (Double, Double) -> StdGen -> ([Double], StdGen)
randomList 0 _ gen = ([], gen)
randomList n bounds gen =
  let (value, gen') = randomR bounds gen
      (rest, gen'') = randomList (n - 1) bounds gen'
   in (value : rest, gen'')

-- | Xavier uniform bounds: (-sqrt(6/(fanIn+fanOut)), sqrt(6/(fanIn+fanOut)))
xavierBounds :: Int -> Int -> (Double, Double)
xavierBounds fanIn fanOut =
  let limit = sqrt (6.0 / fromIntegral (fanIn + fanOut))
   in (-limit, limit)

softMaxGen :: Int -> Int -> Int -> (Double, Double) -> StdGen -> [Matrix Double]
softMaxGen _ _ n _ _ | n <= 0 = []
softMaxGen height width nodes bounds gen =
  (height >< width) vals : softMaxGen height width (nodes - 1) bounds gen'
  where
    (vals, gen') = randomList (height * width) bounds gen

softMaxMultiply :: Matrix Double -> [Matrix Double] -> [Double]
softMaxMultiply input = map (dotProduct input)

main :: IO ()
main = do
    let input = (3><3) [1, 2, 3,
                        0, 1, 4,
                        1, 0, 2]
        kH = 2
        kW = 2
        nodes = 10
        fanIn  = kH * kW
        fanOut = kH * kW
        bounds = xavierBounds fanIn fanOut
        gen    = mkStdGen 42
        (vals, gen') = randomList (kH * kW) bounds gen
        kernel = (kH><kW) vals

        stride = 1
        output = maxPool2x2 (relu $ convolve input kernel stride) 2
        softmax = softMaxGen (rows output) (cols output) nodes bounds gen'
        output' = softMaxMultiply output softmax
    putStrLn "Xavier kernel:"
    print kernel
    putStrLn "Output:"
    print output
    putStrLn "Softmax output:"
    print output'
