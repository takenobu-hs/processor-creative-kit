
{-# OPTIONS -Wall #-}

{-
  trace example

  % runhaskell trace.hs  test.asm
-}

module Main where

import System.Environment(getArgs)
import Language.Pck.Tool


main :: IO ()
main = do (file:_) <- getArgs
          insts <- parseInstFile file
          runDbgIO [TrcInst, TrcReg] []  [(0, insts)] []
--        runDbgIO [TrcInst]         []  [(0, insts)] []



