module Main where

import qualified Command

main :: IO ()
main = Command.parse >>= Command.run
