
{-# OPTIONS -Wall #-}

{-
  run example

  % runhaskell run.hs  test.asm
-}

module Main where

import System.Environment(getArgs)
import Processor.Core
import Processor.Tool


main :: IO ()
main = do (file:_) <- getArgs
          insts <- parseInstFile file
          putStrLn $ dumpCpuState $ run [(0, insts)] []



