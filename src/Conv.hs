module Conv where
import Types
import System.Random


-- Functionality to create a convolutional layer with a specified number of filters
--Using Xavier Initialization for filter weights
--https://365datascience.com/tutorials/machine-learning-tutorials/what-is-xavier-initialization/
--https://www.quora.com/What-is-an-intuitive-explanation-of-the-Xavier-Initialization-for-Deep-Neural-Networks
mkConv :: Int -> Int -> Int -> StdGen -> (Conv, StdGen)
mkConv filterSize channelNumber numFilters g = (Conv
    { numFilters = numFilters
    , filterSize = filterSize
    , channelNumber = channelNumber
    , convolution = output
    }, genAfterWeights)
  where
    limit = xavierLimit fanIn fanOut
    fanIn = channelNumber * filterSize * filterSize
    fanOut = numFilters * filterSize * filterSize
    totalWeights = numFilters * channelNumber * filterSize * filterSize
    (randomWeights, genAfterWeights) = randomList totalWeights (-limit, limit) g
    (output, _) = buildChannels channelNumber filterSize numFilters randomWeights

randomList :: Int -> (Float, Float) -> StdGen -> ([Float], StdGen)
randomList 0 _ gen = ([], gen)
randomList n bounds gen =
  let (value, gen') = randomR bounds gen
      (rest, gen'') = randomList (n - 1) bounds gen'
   in (value : rest, gen'')

buildChannels :: Int -> Int -> Int -> [Float] -> ([[[[Float]]]], [Float])
buildChannels 0 _ _ xs = ([], xs)
buildChannels channelsLeft size numFilters xs = (currentChannel : restChannels, xsAfterRest)
  where
    (currentChannel, xsAfterCurrent) = buildFilters numFilters size xs
    (restChannels, xsAfterRest) = buildChannels (channelsLeft - 1) size numFilters xsAfterCurrent

buildFilters :: Int -> Int -> [Float] -> ([[[Float]]], [Float])
buildFilters 0 _ xs = ([], xs)
buildFilters filtersLeft size xs = (currentFilter : restFilters, xsAfterRest)
  where
    (currentFilter, xsAfterCurrent) = buildRows size size xs
    (restFilters, xsAfterRest) = buildFilters (filtersLeft - 1) size xsAfterCurrent

buildRows :: Int -> Int -> [Float] -> ([[Float]], [Float])
buildRows 0 _ xs = ([], xs)
buildRows rowsLeft rowWidth xs = (row : restRows, xsAfterRest)
  where
    (row, xsAfterRow) = splitAt rowWidth xs
    (restRows, xsAfterRest) = buildRows (rowsLeft - 1) rowWidth xsAfterRow

xavierLimit :: Int -> Int -> Float
xavierLimit fanIn fanOut = sqrt (6 / fromIntegral (fanIn + fanOut))