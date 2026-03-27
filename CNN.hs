import System.Random


data Conv = Conv
  { numFilters :: Int
    , filterSize :: Int
    , channelNumber :: Int
    , convolution :: [[[[Float]]]]
  }
    deriving (Show)

data Channel = Channel
  { height :: Int
    , width :: Int
    , pixels :: [[[Float]]]
  }
    deriving (Show)

filterSizeGlobal :: Int
filterSizeGlobal = 1

channelNumberGlobal :: Int
channelNumberGlobal = 8

numFiltersGlobal :: Int
numFiltersGlobal = 2


main :: IO ()
main = do
    -- Get an initial generator from the system's global state
    g <- getStdGen
    -- Create a convolutional layer with 3 filters
    let (convLayer, _) = mkConv filterSizeGlobal channelNumberGlobal numFiltersGlobal g
    putStrLn $ "Convolution shape (filters, channels, height, width): " ++ show (numFilters convLayer, channelNumber convLayer, filterSize convLayer, filterSize convLayer)
    print convLayer


generateRegions :: Int -> Channel -> [Channel]
generateRegions regionSize channel = [] -- Placeholder for the region generation implementation

forward :: Conv -> Channel -> [Channel]
forward conv channel = [] -- Placeholder for the forward pass implementation

-- Function to create a convolutional layer with a specified number of filters
--Using Xavier Initialization for filter weights
--https://365datascience.com/tutorials/machine-learning-tutorials/what-is-xavier-initialization/
--https://www.quora.com/What-is-an-intuitive-explanation-of-the-Xavier-Initialization-for-Deep-Neural-Networks
mkConv :: Int -> Int -> Int -> StdGen -> (Conv, StdGen)
mkConv filterSize channelNumber numFilters g = (Conv
    { numFilters = numFilters
    , filterSize = filterSize
    , channelNumber = channelNumber
    , convolution = output
    }, finalG)
    where
      limit = xavierLimit channelNumber numFilters filterSize
      totalWeights = numFilters * channelNumber * filterSize * filterSize
      randomWeights = take totalWeights (randomRs (-limit, limit) g)
      (output, finalG) = buildFilters numFilters channelNumber filterSize randomWeights

buildFilters :: Int -> Int -> Int -> [Float] -> ([[[[Float]]]], [Float])
buildFilters 0 _ _ xs = ([], xs)
buildFilters filtersLeft channels size xs = (currentFilter : restFilters, xsAfterRest)
    where
  (currentFilter, xsAfterCurrent) = buildChannels channels size xs
  (restFilters, xsAfterRest) = buildFilters (filtersLeft - 1) channels size xsAfterCurrent

buildChannels :: Int -> Int -> [Float] -> ([[[Float]]], [Float])
buildChannels 0 _ xs = ([], xs)
buildChannels channelsLeft size xs = (currentChannel : restChannels, xsAfterRest)
    where
  (currentChannel, xsAfterCurrent) = buildRows size size xs
  (restChannels, xsAfterRest) = buildChannels (channelsLeft - 1) size xsAfterCurrent

buildRows :: Int -> Int -> [Float] -> ([[Float]], [Float])
buildRows 0 _ xs = ([], xs)
buildRows rowsLeft rowWidth xs = (row : restRows, xsAfterRest)
    where
  (row, xsAfterRow) = splitAt rowWidth xs
  (restRows, xsAfterRest) = buildRows (rowsLeft - 1) rowWidth xsAfterRow

xavierLimit :: Int -> Int -> Int -> Float
xavierLimit channels numFilters filterSize = sqrt (6 / fromIntegral (fanIn + fanOut))
    where
      fanIn = channels * filterSize * filterSize
      fanOut = numFilters * filterSize * filterSize