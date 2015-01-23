
{-# OPTIONS -Wall #-}

{-
  trace example:

    $ runhaskell examples/trace.hs  examples/test0.asm
-}

module Main where

import System.Environment(getArgs)
import Language.Pck.Tool


main :: IO ()
main = do (file:_) <- getArgs
          insts <- parseInstFile file
          runDbgIO [TrcInst]         []  [(0, insts)] []
--        runDbgIO [TrcInst, TrcReg] []  [(0, insts)] []



