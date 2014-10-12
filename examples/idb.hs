
{-# OPTIONS -Wall #-}

{-
  idb example

  % runhaskell idb.hs  test.asm
-}


module Main where

import System.Environment(getArgs)
import Processor.Tool


main :: IO ()
main = do (file:_) <- getArgs
          insts <- parseInstFile file
          runIdbIO [TrcInst] []  [(0, insts)] []


