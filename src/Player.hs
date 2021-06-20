{-# LANGUAGE OverloadedStrings #-}

module Player (play, pause, playPause) where

import qualified DBus
import qualified DBus.Client

play :: IO ()
play = do
  client <- DBus.Client.connectSession
  reply <-
    DBus.Client.call_
      client
      (DBus.methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" "Play")
        { DBus.methodCallDestination = Just "org.mpris.MediaPlayer2.spotify"
        }
  print reply

pause :: IO ()
pause = do
  client <- DBus.Client.connectSession
  reply <-
    DBus.Client.call_
      client
      (DBus.methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" "Pause")
        { DBus.methodCallDestination = Just "org.mpris.MediaPlayer2.spotify"
        }
  print reply

playPause :: IO ()
playPause = do
  client <- DBus.Client.connectSession
  reply <-
    DBus.Client.call_
      client
      (DBus.methodCall "/org/mpris/MediaPlayer2" "org.mpris.MediaPlayer2.Player" "PlayPause")
        { DBus.methodCallDestination = Just "org.mpris.MediaPlayer2.spotify"
        }
  print reply
