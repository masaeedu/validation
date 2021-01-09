module Main where

import Lib
import Text.Pretty.Simple

main :: IO ()
main = do
  _ <- result >>= traverse pPrint
  pure ()
