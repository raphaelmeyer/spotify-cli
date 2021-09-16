{-# LANGUAGE OverloadedStrings #-}

module Player (play, pause, playPause, Error, Result) where

import qualified Control.Concurrent.MVar.Strict as MVar
import Control.Monad.Except
import qualified DBus
import qualified DBus.Client
import Data.Maybe (isJust)
import qualified System.Process as Process
import qualified System.Timeout as Timeout

type Error = String

type Result = ExceptT Error IO

play :: Result ()
play = do
  client <- liftIO DBus.Client.connectSession
  startSpotify client >> spotifyCommand client "Play"

pause :: Result ()
pause = do
  client <- liftIO DBus.Client.connectSession
  spotifyCommand client "Pause"

playPause :: Result ()
playPause = do
  client <- liftIO DBus.Client.connectSession
  startSpotify client >> spotifyCommand client "PlayPause"

spotifyCommand :: DBus.Client.Client -> DBus.MemberName -> Result ()
spotifyCommand client command = do
  reply <-
    liftIO $
      DBus.Client.call
        client
        (DBus.methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" command)
          { DBus.methodCallDestination = Just "org.mpris.MediaPlayer2.spotify"
          }
  case reply of
    Left err -> throwError $ show err
    Right _ -> return ()

startSpotify :: DBus.Client.Client -> Result ()
startSpotify client = do
  running <- spotifyAvailable client
  unless running (spawnSpotify client)

spawnSpotify :: DBus.Client.Client -> Result ()
spawnSpotify client = do
  done <- liftIO MVar.newEmptyMVar
  listener <- liftIO $ DBus.Client.addMatch client DBus.Client.matchAny {DBus.Client.matchMember = Just "NameOwnerChanged"} (notifyStarted done)
  _ <- liftIO $ Process.spawnCommand "spotify"
  started <- liftIO $ Timeout.timeout 10000000 (MVar.takeMVar done)
  liftIO $ DBus.Client.removeMatch client listener
  unless (isJust started) (throwError "Could not start spotify client.")

spotifyAvailable :: DBus.Client.Client -> Result Bool
spotifyAvailable client = do
  reply <-
    liftIO $
      DBus.Client.call
        client
        (DBus.methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
          { DBus.methodCallDestination = Just "org.freedesktop.DBus"
          }
  case reply of
    Left err -> throwError $ show err
    Right methodReturn -> return $ "org.mpris.MediaPlayer2.spotify" `elem` (names :: [String])
      where
        Just names = DBus.fromVariant (head . DBus.methodReturnBody $ methodReturn)

notifyStarted :: MVar.MVar () -> DBus.Signal -> IO ()
notifyStarted done signal =
  when (DBus.toVariant ("org.mpris.MediaPlayer2.spotify" :: String) `elem` DBus.signalBody signal) $ MVar.putMVar done ()
