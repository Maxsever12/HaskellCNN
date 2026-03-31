module Regions where
import Types

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