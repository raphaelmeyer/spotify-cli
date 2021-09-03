{-# LANGUAGE OverloadedStrings #-}

module Player (play, pause, playPause) where

import qualified Control.Concurrent.MVar.Strict as MVar
import Control.Monad (when)
import qualified DBus
import qualified DBus.Client
import Data.Maybe (isJust)
import qualified System.Process as Process
import qualified System.Timeout as Timeout

play :: IO (Either String ())
play = do
  client <- DBus.Client.connectSession
  startSpotify client
  _ <- spotifyCommand client "Play"
  return (Right ())

pause :: IO (Either String ())
pause = do
  client <- DBus.Client.connectSession
  _ <- spotifyCommand client "Pause"
  return (Right ())

playPause :: IO (Either String ())
playPause = do
  client <- DBus.Client.connectSession
  startSpotify client
  _ <- spotifyCommand client "PlayPause"
  return (Right ())

spotifyCommand :: DBus.Client.Client -> DBus.MemberName -> IO DBus.MethodReturn
spotifyCommand client command =
  DBus.Client.call_
    client
    (DBus.methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" command)
      { DBus.methodCallDestination = Just "org.mpris.MediaPlayer2.spotify"
      }

-- catch ClientError
-- return type ? Maybe () ? Either String () ?

startSpotify :: DBus.Client.Client -> IO ()
startSpotify client = do
  running <- spotifyAvailable client
  if running
    then return ()
    else do
      done <- MVar.newEmptyMVar
      listener <- DBus.Client.addMatch client DBus.Client.matchAny {DBus.Client.matchMember = Just "NameOwnerChanged"} (notifyStarted done)
      _ <- Process.spawnCommand "spotify"
      started <- Timeout.timeout 10000000 (MVar.takeMVar done)
      DBus.Client.removeMatch client listener
      when (isJust started) $ return ()

spotifyAvailable :: DBus.Client.Client -> IO Bool
spotifyAvailable client = do
  reply <-
    DBus.Client.call_
      client
      (DBus.methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
        { DBus.methodCallDestination = Just "org.freedesktop.DBus"
        }

  -- call_ => call -> Either DBus.MethodError DBus.MethodReturn

  let Just names = DBus.fromVariant (head . DBus.methodReturnBody $ reply)

  return ("org.mpris.MediaPlayer2.spotify" `elem` (names :: [String]))

notifyStarted :: MVar.MVar () -> DBus.Signal -> IO ()
notifyStarted done signal = do
  when (DBus.toVariant ("org.mpris.MediaPlayer2.spotify" :: String) `elem` DBus.signalBody signal) $ MVar.putMVar done ()
