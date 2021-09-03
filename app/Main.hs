module Main where

import qualified Command
import qualified System.Exit as Exit

main :: IO ()
main = Command.parse >>= Command.run >>= either exitWithError (\_ -> Exit.exitSuccess)

exitWithError :: String -> IO ()
exitWithError reason = do
  putStrLn reason
  Exit.exitWith (Exit.ExitFailure 17)
