module Main where

import Example
import Text.Pretty.Simple

main :: IO ()
main = do
  _ <- result >>= traverse pPrint
  pure ()
