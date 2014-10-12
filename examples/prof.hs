
{-# OPTIONS -Wall #-}

{-
  profile example

  % runhaskell profile.hs  test.asm
-}

module Main where

import System.Environment(getArgs)
import Processor.Tool


main :: IO ()
main = do (file:_) <- getArgs
          insts <- parseInstFile file
          runProfIO [ProfInst, ProfCall, ProfLoad, ProfStore, ProfBranch]
                    [(0, insts)] []




