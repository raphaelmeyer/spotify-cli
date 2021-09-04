{-# LANGUAGE OverloadedStrings #-}

module Player (play, pause, playPause) where

import qualified Control.Concurrent.MVar.Strict as MVar
import Control.Monad (unless, when)
import Control.Monad.Trans.Except
import qualified DBus
import qualified DBus.Client
import Data.Maybe (isJust)
import qualified System.Process as Process
import qualified System.Timeout as Timeout

play :: IO (Either String ())
play = do
  client <- DBus.Client.connectSession
  _ <- startSpotify client
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
  _ <- startSpotify client
  _ <- spotifyCommand client "PlayPause"
  return (Right ())

spotifyCommand :: DBus.Client.Client -> DBus.MemberName -> IO DBus.MethodReturn
spotifyCommand client command =
  DBus.Client.call_
    client
    (DBus.methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" command)
      { DBus.methodCallDestination = Just "org.mpris.MediaPlayer2.spotify"
      }

startSpotify :: DBus.Client.Client -> IO (Either String ())
startSpotify client = runExceptT $ do
  running <- ExceptT $ spotifyAvailable client
  unless running (ExceptT (spawnSpotify client))

spawnSpotify :: DBus.Client.Client -> IO (Either String ())
spawnSpotify client = do
  done <- MVar.newEmptyMVar
  listener <- DBus.Client.addMatch client DBus.Client.matchAny {DBus.Client.matchMember = Just "NameOwnerChanged"} (notifyStarted done)
  _ <- Process.spawnCommand "spotify"
  started <- Timeout.timeout 10000000 (MVar.takeMVar done)
  DBus.Client.removeMatch client listener
  if isJust started
    then return $ Right ()
    else return $ Left "Could not start spotify client."

spotifyAvailable :: DBus.Client.Client -> IO (Either String Bool)
spotifyAvailable client = do
  reply <-
    DBus.Client.call
      client
      (DBus.methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
        { DBus.methodCallDestination = Just "org.freedesktop.DBus"
        }

  case reply of
    Left err -> return (Left . show $ err)
    Right methodReturn ->
      return (Right ("org.mpris.MediaPlayer2.spotify" `elem` (names :: [String])))
      where
        Just names = DBus.fromVariant (head . DBus.methodReturnBody $ methodReturn)

notifyStarted :: MVar.MVar () -> DBus.Signal -> IO ()
notifyStarted done signal = do
  when (DBus.toVariant ("org.mpris.MediaPlayer2.spotify" :: String) `elem` DBus.signalBody signal) $ MVar.putMVar done ()
