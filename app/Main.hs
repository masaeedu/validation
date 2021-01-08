module Main where

import Lib
import Text.Pretty.Simple

main :: IO ()
main = do
  _ <- traverse pPrint result
  pure ()
