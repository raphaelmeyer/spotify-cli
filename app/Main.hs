module Main where

import Options.Applicative

data Actions = Play | Pause | PlayPause | CurrentSong deriving (Eq, Show)

actions :: Parser Actions
actions =
  hsubparser
    ( command "play" (info (pure Play) (progDesc "Start playback"))
        <> command "pause" (info (pure Pause) (progDesc "Pause playback"))
        <> command "toggle" (info (pure PlayPause) (progDesc "Toggle play and pause"))
        <> commandGroup "Playback commands:"
        <> metavar "Playback COMMAND"
    )
    <|> hsubparser
      ( command "daemon" (info (pure CurrentSong) (progDesc "Start daemon"))
          <> commandGroup "Daemon commands:"
          <> metavar "Daemon COMMAND"
      )

options :: ParserInfo Actions
options =
  info
    (actions <**> helper)
    ( fullDesc
        <> progDesc "Spotify CLI Tool"
        <> header "Control spotify client from the command line."
    )

main :: IO ()
main = execParser options >>= print
