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
  )
  where

import List (inits, intersperse)
import Directory
import Monad (guard, when)
import System (getEnv, system)

-- alle pfade sind relativ zu $HOME 
data Datei = Datei{ pfad :: [String]
                  , name :: String
                  , relativzahl :: Int
                  }

instance Show Datei where
    show d = "$HOME/" ++ inner d

inner :: Datei -> String -- ohne $HOME-prefix
inner d =  concat $ intersperse "/" (pfad d ++ [name d])

home :: Datei -> IO FilePath
home d = do
    prefix <- getEnv "HOME"
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
    system $ unwords [ "chmod",  "go+r", h ]    
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
    h <- getEnv "HOME"
    sequence_ $ do
        prefix <- inits $ h : pfad d
	guard $ not $ null prefix
	let path = concat $ intersperse "/" prefix
	return $ do ok <- doesDirectoryExist path
		    when ( not ok ) $ do 
--                     putStrLn $ "creating directory " ++ show path
		       createDirectory path
		       system $ unwords [ "chmod",  "go+rx", path ]
		       return ()
