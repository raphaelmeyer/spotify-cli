module Player (play, pause, playPause) where

play :: IO ()
play = print "play ..."

pause :: IO ()
pause = print "pause ..."

playPause :: IO ()
playPause = print "play/pause ..."
