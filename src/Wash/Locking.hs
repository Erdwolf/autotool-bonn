module Wash.Locking (obtainLock, releaseLock) where

import Wash.Auxiliary
import Directory
import IO
import System

obtainLock  :: FilePath -> IO ()
releaseLock :: FilePath -> IO ()

lockPath name = name ++ ".lockdir"

obtainLock name =
  assertDirectoryExists (lockPath name)
                        (system "sleep 1" >> obtainLock name)

releaseLock name =
  removeDirectory (lockPath name)
