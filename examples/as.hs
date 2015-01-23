
{-# OPTIONS -Wall #-}

{-
  assemble example:

    $ runhaskell examples/as.hs  examples/test0.asm
-}

module Main where

import System.Environment(getArgs)
import Language.Pck.Tool


main :: IO ()
main = do (file:_) <- getArgs
          insts <- parseInstFile file
          print insts



