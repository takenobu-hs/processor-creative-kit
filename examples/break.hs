
{-
  break example:

    $ runhaskell examples/break.hs  examples/test0.asm
-}

module Main where

import System.Environment(getArgs)
import Language.Pck.Cpu
import Language.Pck.Tool


main :: IO ()
main = do (file:_) <- getArgs
          insts <- parseInstFile file
          runDbgIO [TrcInst] [(BrkPc BEQ 2)]      [(0, insts)] []
--        runDbgIO [TrcInst] [(BrkGReg R1 BGT 2)] [(0, insts)] []
--        runDbgIO [TrcInst] [(BrkDmem 0 BGT 0)]  [(0, insts)] []
--        runDbgIO [TrcInst] [(BrkPc BEQ 2), (BrkPc BEQ 5)] [(0, insts)] []



