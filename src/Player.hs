{-# LANGUAGE OverloadedStrings #-}

module Player (play, pause, playPause) where

import qualified Control.Concurrent.MVar.Strict as MVar
import Control.Exception (Exception, bracket, throw)
import Control.Monad (unless, when)
import qualified DBus
import qualified DBus.Client
import Data.Maybe (isJust)
import qualified System.Process as Process
import qualified System.Timeout as Timeout

data PlayerException = CommandFailed | DBusFailure | StartSpotifyFailed deriving (Show)

instance Exception PlayerException

play :: IO ()
play = withDBus (\client -> startSpotify client >> spotifyCommand client "Play")

pause :: IO ()
pause = withDBus (`spotifyCommand` "Pause")

playPause :: IO ()
playPause = withDBus (`spotifyCommand` "PlayPause")

withDBus :: (DBus.Client.Client -> IO ()) -> IO ()
withDBus = bracket DBus.Client.connectSession DBus.Client.disconnect

spotifyCommand :: DBus.Client.Client -> DBus.MemberName -> IO ()
spotifyCommand client command = do
  reply <-
    DBus.Client.call
      client
      (DBus.methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" command)
        { DBus.methodCallDestination = Just "org.mpris.MediaPlayer2.spotify"
        }
  case reply of
    Left _ -> throw CommandFailed
    Right _ -> return ()

startSpotify :: DBus.Client.Client -> IO ()
startSpotify client = do
  running <- spotifyAvailable client
  unless running $ spawnSpotify client

spawnSpotify :: DBus.Client.Client -> IO ()
spawnSpotify client = do
  done <- MVar.newEmptyMVar
  listener <- DBus.Client.addMatch client DBus.Client.matchAny {DBus.Client.matchMember = Just "NameOwnerChanged"} (notifyStarted done)
  _ <- Process.spawnCommand "spotify"
  started <- Timeout.timeout 10000000 (MVar.takeMVar done)
  DBus.Client.removeMatch client listener
  unless (isJust started) $ throw StartSpotifyFailed

spotifyAvailable :: DBus.Client.Client -> IO Bool
spotifyAvailable client = do
  reply <-
    DBus.Client.call
      client
      (DBus.methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
        { DBus.methodCallDestination = Just "org.freedesktop.DBus"
        }
  case reply of
    Left _ -> throw DBusFailure
    Right methodReturn -> return $ "org.mpris.MediaPlayer2.spotify" `elem` (names :: [String])
      where
        Just names = DBus.fromVariant (head . DBus.methodReturnBody $ methodReturn)

notifyStarted :: MVar.MVar () -> DBus.Signal -> IO ()
notifyStarted done signal =
  when (DBus.toVariant ("org.mpris.MediaPlayer2.spotify" :: String) `elem` DBus.signalBody signal) $ MVar.putMVar done ()
