module Wash.Auxiliary where

import IO
import System
import Directory

protectedGetEnv :: String -> String -> IO String
protectedGetEnv var deflt =
	catch (getEnv var) (const $ return deflt)

readFileNonExistent fileName def =
  do existent <- doesFileExist fileName
     if existent then readFile fileName else return def

assertDirectoryExists :: FilePath -> IO () -> IO ()
assertDirectoryExists dirname existsAction =
  catch (createDirectory dirname)
        (\ ioe -> if isAlreadyExistsError ioe
	          then existsAction
		  else ioError ioe)

writeDebugFile :: String -> String -> IO ()
writeDebugFile filename str =
  do writeFile filename str
     system ("chmod 666 " ++ filename)
     return ()
