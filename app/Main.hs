module Main where

import qualified Command
import Control.Exception (IOException, catch)
import qualified System.Exit
import qualified System.IO

main :: IO ()
main = do
  action <- Command.parse
  catch (Command.run action) exitWithError
  System.Exit.exitSuccess

exitWithError :: IOException -> IO ()
exitWithError e = do
  System.IO.hPutStrLn System.IO.stderr $ show e
  System.Exit.exitWith (System.Exit.ExitFailure 1)
