module Daemon (start, stop) where

import Control.Monad.Except

type Error = String

type Result = ExceptT Error IO

start :: Result ()
start = liftIO $ print "start daemon..."

stop :: Result ()
stop = liftIO $ print "slay daemon!"
