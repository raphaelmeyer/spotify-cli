module Command (parse, run) where

import Control.Monad.Except (runExceptT)
import qualified Daemon as D
import Options.Applicative
import qualified Player as P

data DaemonAction = Start | Stop deriving (Eq, Show)

data PlayerAction = Play | Pause | PlayPause deriving (Eq, Show)

data Action = Player PlayerAction | Daemon DaemonAction deriving (Eq, Show)

parse :: IO Action
parse = execParser options

run :: Action -> IO (Either String ())
run (Player a) = case a of
  Play -> runExceptT P.play
  Pause -> runExceptT P.pause
  PlayPause -> runExceptT P.playPause
run (Daemon a) = case a of
  Start -> D.start >> return (Right ())
  Stop -> D.stop >> return (Right ())

actions :: Parser Action
actions =
  hsubparser
    ( command "play" (info (pure (Player Play)) (progDesc "Start playback"))
        <> command "pause" (info (pure (Player Pause)) (progDesc "Pause playback"))
        <> command "toggle" (info (pure (Player PlayPause)) (progDesc "Toggle play and pause"))
        <> command "daemon" (info daemonActions (progDesc "Start/Stop daemon"))
    )

daemonActions :: Parser Action
daemonActions =
  hsubparser
    ( command "start" (info (pure (Daemon Start)) (progDesc "Start daemon"))
        <> command "stop" (info (pure (Daemon Stop)) (progDesc "Stop daemon"))
    )

options :: ParserInfo Action
options =
  info
    (actions <**> helper)
    ( fullDesc
        <> progDesc "Spotify CLI Tool"
        <> header "Control spotify client from the command line."
    )
