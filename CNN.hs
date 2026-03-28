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
    , pixels :: [[Float]]
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
    let channel =
          Channel
            { height = 4
            , width = 4
            , pixels = [ [1, 2, 3, 4]
                       , [5, 6, 7, 8]
                       , [9, 10, 11, 12]
                       , [13, 14, 15, 16]
                       ]
            }
    print $ forward (convolution convLayer) [generateRegions 3 channel]
  
    


forward :: [[[[Float]]]] -> [[[Channel]]] -> [Channel]
forward conv [] = []
forward [] _ = []
forward (channelFilter:channelFilters) (channel:channels) = convolveFilter channelFilter channel ++ forward channelFilters channels

convolveFilter :: [[[Float]]] -> [[Channel]] -> [Channel]
convolveFilter filter [] = []
convolveFilter [] _ = []
convolveFilter (filter:filters) channel = applyFilter filter channel : convolveFilter filters channel

applyFilter :: [[Float]] -> [[Channel]] -> Channel
applyFilter filter channels = Channel {
  height = length channels 
, width = if null channels then 0 else length (head channels)
, pixels = applyFilterToRows filter channels
}

applyFilterToRows :: [[Float]] -> [[Channel]] -> [[Float]]
applyFilterToRows _ [] = []
applyFilterToRows filter (channels:channelss) = applyFilterToPixels filter channels : applyFilterToRows filter channelss

applyFilterToPixels :: [[Float]] -> [Channel] -> [Float]
applyFilterToPixels _ [] = []
applyFilterToPixels filter (channel:channels) =
  applyKernel filter (pixels channel) : applyFilterToPixels filter channels

applyKernel :: [[Float]] -> [[Float]] -> Float
applyKernel filter region = sum (zipWith multiplyRow filter region)
  where
    multiplyRow filterRow regionRow = sum (zipWith (*) filterRow regionRow)



padChannel :: Int -> Channel -> Channel
padChannel pad channel = Channel
    { height = height channel + 2 * pad
    , width = width channel + 2 * pad
    , pixels = paddedPixels
    }
    where
      paddedPixels = replicate pad emptyRow ++ paddedRows ++ replicate pad emptyRow
      paddedRows = map (\row -> replicate pad 0 ++ row ++ replicate pad 0) (pixels channel)
      emptyRow = replicate (width channel + 2 * pad) 0

generateRegions :: Int -> Channel -> [[Channel]]
generateRegions regionSize channel
  | regionSize <= 0 = []
  | even regionSize = []
  | otherwise = generateRegionsHelper regionSize (pixels paddedChannel)
  where
    pad = regionSize `div` 2
    paddedChannel = padChannel pad channel

generateRegionsHelper :: Int -> [[Float]] -> [[Channel]]
generateRegionsHelper _ [] = []
generateRegionsHelper regionSize rows
  | length rows < regionSize = []
  | otherwise =
      rowGroupChannels regionSize (take regionSize rows)
        : generateRegionsHelper regionSize (tail rows)

rowGroupChannels :: Int -> [[Float]] -> [Channel]
rowGroupChannels regionSize rows
  | length rows < regionSize = []
  | any (\r -> length r < regionSize) rows = []
  | otherwise =
      Channel
        { height = regionSize
        , width = regionSize
        , pixels = map (take regionSize) rows
        }
        : rowGroupChannels regionSize (map tail rows)



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
    }, genAfterWeights)
  where
    limit = xavierLimit channelNumber numFilters filterSize
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

xavierLimit :: Int -> Int -> Int -> Float
xavierLimit channels numFilters filterSize = sqrt (6 / fromIntegral (fanIn + fanOut))
    where
      fanIn = channels * filterSize * filterSize
      fanOut = numFilters * filterSize * filterSize