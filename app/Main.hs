module Main where

import qualified Command
import qualified System.Exit
import qualified System.IO

main :: IO ()
main = Command.parse >>= Command.run >>= either exitWithError (\_ -> System.Exit.exitSuccess)

exitWithError :: String -> IO ()
exitWithError reason = do
  System.IO.hPutStrLn System.IO.stderr reason
  System.Exit.exitWith (System.Exit.ExitFailure 17)
