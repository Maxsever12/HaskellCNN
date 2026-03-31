module Types where
    
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

newtype Weights = Weights
  { weightValues :: [Float]
  }
    deriving (Show)

newtype Biases = Biases
  { biasValues :: [Float]
  }
    deriving (Show)