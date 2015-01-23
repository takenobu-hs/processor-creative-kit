
{-# OPTIONS -Wall #-}

{-
  run example:

    $ runhaskell examples/run.hs  examples/test0.asm
-}

module Main where

import System.Environment(getArgs)
import Language.Pck.Cpu
import Language.Pck.Tool


main :: IO ()
main = do (file:_) <- getArgs
          insts <- parseInstFile file
          putStrLn $ dumpCpuState $ run [(0, insts)] []



