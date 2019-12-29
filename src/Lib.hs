{-# LANGUAGE ScopedTypeVariables   #-}

module Lib ( 
  run
) where

import Blog.UnfoldExample (extractState, serviceCall)
import Blog.UnfoldWith (unfoldWith, CursorState(NewCursor))
import Control.Monad.Loops (unfoldrM)
import Data.List (intercalate)

run :: IO ()
run = 
  let resultsIO :: IO [String] = unfoldrM (unfoldWith extractState serviceCall) NewCursor
      stringyfied :: IO String = (intercalate "," . take 3) <$> resultsIO
  in stringyfied >>= putStrLn

