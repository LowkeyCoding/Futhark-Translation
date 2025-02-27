module Fresh where

import Control.Monad.State

-- Define a type for generating fresh names
type NameGenerator = State Int

-- Function to generate a fresh name with a custom prefix
freshName :: String -> NameGenerator String
freshName prefix = do
  counter <- get          -- Get the current counter value
  put (counter + 1)       -- Increment the counter
  return (prefix ++ "_" ++ show counter)  -- Return a fresh name like "prefix0", "prefix1", etc.

-- Helper function to run the name generator
runNameGenerator :: NameGenerator a -> a
runNameGenerator generator = evalState generator 0  -- Start with counter = 0
