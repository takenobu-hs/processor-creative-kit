
{-# OPTIONS -Wall #-}

{-
  profile example:

    $ runhaskell examples/prof.hs  examples/test0.asm
-}

module Main where

import System.Environment(getArgs)
import Language.Pck.Tool


main :: IO ()
main = do (file:_) <- getArgs
          insts <- parseInstFile file
          runProfIO [ProfInst, ProfCall, ProfLoad, ProfStore, ProfBranch]
                    [(0, insts)] []




