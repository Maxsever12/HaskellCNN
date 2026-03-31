module Forward where
import Types

forward :: [[[[Float]]]] -> [[[Channel]]] -> [Channel]
forward conv [] = []
forward [] _ = []
forward (channelFilter:channelFilters) (channel:channels) = convolveFilter channelFilter channel ++ forward channelFilters channels

convolveFilter :: [[[Float]]] -> [[Channel]] -> [Channel]
convolveFilter krnl [] = []
convolveFilter [] _ = []
convolveFilter (krnl:krnls) channel = applyFilter krnl channel : convolveFilter krnls channel

applyFilter :: [[Float]] -> [[Channel]] -> Channel
applyFilter knrl channels = Channel {
  height = length channels 
, width = if null channels then 0 else length (head channels)
, pixels = applyFilterToRows knrl channels
}

applyFilterToRows :: [[Float]] -> [[Channel]] -> [[Float]]
applyFilterToRows _ [] = []
applyFilterToRows knrl (channels:channelss) = applyFilterToPixels knrl channels : applyFilterToRows knrl channelss

applyFilterToPixels :: [[Float]] -> [Channel] -> [Float]
applyFilterToPixels _ [] = []
applyFilterToPixels knrl (channel:channels) =
  applyKernel knrl (pixels channel) : applyFilterToPixels knrl channels

applyKernel :: [[Float]] -> [[Float]] -> Float
applyKernel knrl region = sum (zipWith multiplyRow knrl region)
  where
    multiplyRow knrlRow regionRow = sum (zipWith (*) knrlRow regionRow)
