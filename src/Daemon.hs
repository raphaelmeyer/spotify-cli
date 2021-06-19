module Daemon (start, stop) where

start :: IO ()
start = print "start daemon..."

stop :: IO ()
stop = print "slay daemon!"
