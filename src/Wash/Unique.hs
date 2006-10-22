-- © 2001 Peter Thiemann
module Wash.Unique where

import Random
import Directory
import Wash.Auxiliary
import Data.List
import Control.Monad
import Wash.Locking

import Wash.Config

registryFile = registryDir ++ "REGISTRY"

inventStdKey = inventKey 20 stdKeyChars
stdKeyChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

inventKey len chars =
  do g <- newStdGen
     let candidate = take len $ map (chars !!) $ randomRs (0, length chars - 1) g
     obtainLock registryFile
     registry <- readRegistry
     let passwords = lines registry
     if candidate `elem` passwords 
       then do releaseLock registryFile
	       inventKey len chars
       else do appendFile registryFile (candidate ++ "\n")
	       releaseLock registryFile
	       return candidate

inventFilePath =
  do key <- inventStdKey
     return (registryDir ++ key)

readRegistry =
  let registryPath = init registryDir in
  do assertDirectoryExists registryPath (return ())
     readFileNonExistent registryFile ""
