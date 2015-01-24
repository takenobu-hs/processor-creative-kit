
{-# OPTIONS -Wall #-}

{-
  idb example:

    $ runhaskell examples/idb2.hs  examples/test0.asm
    (idb) s
    (idb) info reg
    (idb) x/8 0x10
    (idb) p *0x10
    (idb) p *0x10 = 0x5
    (idb) b 0x4
    (idb) watch *0x80 != 10
    (idb) watch pc > 3
    (idb) watch r7 == 3
    (idb) info b
    (idb) c
    (idb) help
    (idb) q

-}


module Main where

import System.Environment(getArgs)
import Language.Pck.Tool


main :: IO ()
main = do (file:_) <- getArgs
          insts <- parseInstFile file
--        runIdbIO [TrcInst]                [] [(0, insts)] []
          runIdbIO [TrcInst, TrcReg]        [] [(0, insts)] []


