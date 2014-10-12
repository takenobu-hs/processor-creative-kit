
{-# OPTIONS -Wall #-}

{-
  trace example

  % runhaskell trace.hs  test.asm
-}

module Main where

import System.Environment(getArgs)
import Processor.Tool


main :: IO ()
main = do (file:_) <- getArgs
          insts <- parseInstFile file
          runDbgIO [TrcInst] []  [(0, insts)] []



