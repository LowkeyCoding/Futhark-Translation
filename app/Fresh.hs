module Fresh where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)

-- Define a type for generating fresh names, using a map to track per-prefix counters
type NameGenerator = State (Map String Int)

-- Function to generate a fresh name with a custom prefix, using individual counters per prefix
freshName :: String -> NameGenerator String
freshName prefix = do
  counts <- get -- Get the current map of counters
  let current = Map.findWithDefault 0 prefix counts -- Retrieve or default the counter for the prefix
  put (Map.insert prefix (current + 1) counts) -- Update the counter for the prefix
  return (prefix ++ "_" ++ show current) -- Return the fresh name

-- Helper function to run the name generator, starting with an empty map of counters
runNameGenerator :: NameGenerator a -> a
runNameGenerator generator = evalState generator Map.empty