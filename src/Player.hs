{-# LANGUAGE OverloadedStrings #-}

module Player (play, pause, playPause) where

import qualified DBus
import qualified DBus.Client

play :: IO ()
play = do
  client <- DBus.Client.connectSession
  reply <- spotifyCommand client "Play"
  print reply

pause :: IO ()
pause = do
  client <- DBus.Client.connectSession
  reply <- spotifyCommand client "Pause"
  print reply

playPause :: IO ()
playPause = do
  client <- DBus.Client.connectSession
  reply <- spotifyCommand client "PlayPause"
  print reply

spotifyCommand :: DBus.Client.Client -> DBus.MemberName -> IO DBus.MethodReturn
spotifyCommand client command =
  DBus.Client.call_
    client
    (DBus.methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" command)
      { DBus.methodCallDestination = Just "org.mpris.MediaPlayer2.spotify"
      }
