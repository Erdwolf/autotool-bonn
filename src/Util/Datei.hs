module Util.Datei 

-- $Id$

 (  schreiben
  , anhaengen
  , lesen
  , dirlesen
  , relativieren
  , erzeugeVerzeichnisse
  , home
  , Datei (..)
  , inner
  , perm
  )
  where

import List (inits, intersperse)
import Directory
import Monad (guard, when)
import System (getEnv, system)
import qualified Posix
import qualified Exception

-- alle pfade sind relativ zu $HOME, falls das existiert
-- in CGI-skripten existiert es nicht (?)
-- dann relativ zu /home/$(posix.getloginname)

data Datei = Datei{ pfad :: [String]
                  , name :: String
                  , relativzahl :: Int
                  }

instance Show Datei where
    show d = "$HOME/" ++ inner d

inner :: Datei -> String -- ohne $HOME-prefix
inner d =  concat $ intersperse "/" (pfad d ++ [name d])

home_dir :: IO FilePath
home_dir = getEnv "HOME"
    `Exception.catch` \ any -> do
           user <- Posix.getEffectiveUserName
	   return $ "/home/" ++ user

home :: Datei -> IO FilePath
home d = do
    prefix <- home_dir
    return $ prefix ++ "/" ++ inner d

anhaengen :: Datei -> String -> IO()
anhaengen d inhalt = do
    createDir d
    h <- home d
    appendFile h inhalt

schreiben :: Datei -> String -> IO()
schreiben d inhalt = do
    createDir d
    h <- home d
    writeFile h inhalt
    perm "go+r" h
    return ()

perm :: String -> FilePath -> IO ()
perm flags f = do
    system $ unwords [ "chmod",  flags, f ]    
    return ()


lesen :: Datei -> IO(String)
lesen d  = do
    h <- home d
    readFile h

dirlesen :: Datei -> IO [String]
dirlesen d  = do
    h <- home d
    getDirectoryContents h

----------------------------------------------------------------------------


relativieren :: Datei -> String
relativieren d = concat $ intersperse "/" ((drop (relativzahl d) (pfad d)) ++ [name d])
    

erzeugeVerzeichnisse :: Datei -> IO ()
erzeugeVerzeichnisse = createDir

createDir :: Datei -> IO ()
createDir d = do
    h <- home_dir
    sequence_ $ do
        prefix <- inits $ h : pfad d
	guard $ not $ null prefix
	let path = concat $ intersperse "/" prefix
	return $ do ok <- doesDirectoryExist path
		    when ( not ok ) $ do 
--                     putStrLn $ "creating directory " ++ show path
		       createDirectory path
		       perm "go+rx" path 
		       return ()
