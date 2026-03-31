import System.Random
import Types
import Conv
import Regions
import Forward
import Pooling
import Control.Arrow (ArrowApply(app))

main :: IO ()
main = do
    -- Get an initial generator from the system's global state
    g <- getStdGen
    -- Create a convolutional layer with 3 filters
    let filterSize = 5
    let channelNumber = 1
    let numFilters = 2
    let nodes = 10
    let (convLayer, g') = mkConv filterSize channelNumber numFilters g
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
    let c = forward (convolution convLayer) [generateRegions filterSize channel]
    print "Max Pooling Applied:"
    print c
    let pooled = map maxPool2 c
    let size = height $ head pooled
    let size2 = width $ head pooled
    let inputLength = size * size2 * numFilters
    let limit = xavierLimit (size * size2 * numFilters) nodes
    let (list,_) = randomList inputLength (-limit, limit) g'
    let weights = initializeWeights size size2 numFilters nodes g'
    let biases = initializeBiases nodes
    print "Weights for Fully Connected Layer:"
    print weights
    print "Random Weights for Fully Connected Layer:"
    let nodes = applyWeightsAndBiases weights biases (concat(concatMap pixels pooled))
    print $ length (concat(concatMap pixels pooled))
    print $ length (weightValues weights)
    print $ length (biasValues biases)
    print nodes




applyWeightsAndBiases :: Weights -> Biases -> [Float] -> [Float]
applyWeightsAndBiases w b inputs =
    zipWith (+) weighted (biasValues b)
    where
        weighted = applyWeights (chunkWeights w (length inputs)) inputs

applyWeights :: [[Float]] -> [Float] -> [Float]
applyWeights [] _ = []
applyWeights (w:ws) inputs = applied : applyWeights ws inputs
    where
        applied = sum (zipWith (*) w inputs)

chunkWeights :: Weights -> Int -> [[Float]]
chunkWeights w = chunkList $ weightValues w

chunkList :: [Float] -> Int -> [[Float]]
chunkList [] _ = []
chunkList xs n = take n xs : chunkList (drop n xs) n

--Function to initialize biases for a fully connected layer
initializeBiases :: Int -> Biases
initializeBiases size = Biases { biasValues = replicate size 0 }

-- Function to initialize weights for a fully connected layer using Xavier Initialization
-- Uses existing functionality from Conv.hs to generate random weights
initializeWeights :: Int -> Int-> Int -> Int -> StdGen -> Weights
initializeWeights size size2 numFilters nodes g' =
    Weights { weightValues = weightsFlattened
         }
        where (weights,_) = buildRows (size*size2*numFilters) nodes list
              weightsFlattened = concat weights
              (list,_) = randomList inputLength (-limit, limit) g'
              limit = xavierLimit (size * size2 * numFilters) nodes
              inputLength = size * size2 * numFilters * nodes
