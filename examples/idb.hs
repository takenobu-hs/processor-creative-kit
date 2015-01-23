
{-# OPTIONS -Wall #-}

{-
  idb example:

    $ runhaskell examples/idb.hs  examples/test0.asm
-}


module Main where

import System.Environment(getArgs)
import Language.Pck.Tool


main :: IO ()
main = do (file:_) <- getArgs
          insts <- parseInstFile file
          runIdbIO [TrcInst]                [] [(0, insts)] []
--        runIdbIO [TrcInst, TrcReg]        [] [(0, insts)] []


