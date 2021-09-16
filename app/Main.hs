module Main where

import qualified Command
import Control.Monad.Except
import qualified System.Exit
import qualified System.IO

main :: IO ()
main =
  Command.parse >>= (\action -> runExceptT $ (Command.run action)) >>= either exitWithError (const System.Exit.exitSuccess)

exitWithError :: String -> IO ()
exitWithError reason = do
  System.IO.hPutStrLn System.IO.stderr reason
  System.Exit.exitWith (System.Exit.ExitFailure 17)
