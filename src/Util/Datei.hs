module Util.Datei 

--  $Id$

(  schreiben, mschreiben
, anhaengen
, lesen
, existiert
, dirlesen
, dirgehen
, erzeugeVerzeichnisse
, home
, Datei (..)
, inner, outer, internalize
, perm
) 

where

import Data.List (inits, intersperse)
import Directory
import Control.Monad (guard, when)
import System (getEnv, system)
import Char (isAlphaNum)
import qualified System.Posix
import qualified Control.Exception

import Util.Datei.Base

-- | alle pfade sind relativ zu Util.Datei.Base
data Datei = Datei{ pfad :: [String]
                  , name :: String
                  , extension :: String
                  }

instance Show Datei where
    show d = "$HOME/" ++ inner d

namex d = name d 
       ++ if null (extension d) then "" else "." ++ extension d

-- | ohne $HOME-prefix
inner :: Datei -> String 
inner d =  concat 
	$ intersperse "/" 
	$ pfad d ++ [ namex d ]

-- | das kann als arg. für CGI verwendet werden:
-- dann darf im filenamen natürlich kein komma stehen
outer :: Datei -> String
outer d =  concat 
	$ intersperse "," 
	$ pfad d ++ [namex d ]
    
cutter :: Char -> String -> [ String ]
cutter x cs = words $ map trans cs
    where trans c = if c == x then ' ' else c

internalize :: String -> Datei
internalize cs = 
    let ws = cutter ',' cs :: [ String ]
	(f, e) = case cutter '.' $ last ws of
            [ f    ] -> ( f, "" )
	    [ f, e ] -> ( f, e  )
    in  Datei { pfad = init ws
	      , name = f
	      , extension = e
	      }

home_dir :: IO FilePath
home_dir = do
	    return Util.Datei.Base.base

-- | mit großer vorsicht: über das argument von Get.cgi
-- könnte jemand von außen files zu lesen probieren.
-- inklusive aller tricks mit "..\/.."  usw.
-- deswegen vor jedem zugriff testen
sanity :: Datei -> IO ()
sanity d = 
    when ( not $ all isok
	       $ concat $ pfad d ++ [ name d , extension d ] 
	 ) $ error 
           $ "strange zeichen in dateiname: " ++ show d

isok :: Char -> Bool
isok c = isAlphaNum c || c `elem` "-_"


home :: Datei -> IO FilePath
home d = do
    sanity d
    prefix <- home_dir
    return $ prefix ++ "/" ++ inner d

anhaengen :: Datei -> String -> IO ()
anhaengen d inhalt = do
    createDir d
    h <- home d
    appendFile h inhalt

schreiben :: Datei -> String -> IO FilePath
schreiben d inhalt = do
    createDir d
    h <- home d
    writeFile h inhalt
    perm "go+r" h
    return h

mschreiben :: Datei -> Maybe String -> IO (Maybe FilePath)
mschreiben d (Just inhalt) = do
    f <- schreiben d inhalt
    return $ Just f
mschreiben d Nothing = do
    return Nothing

dirgehen :: Datei -> IO ()
dirgehen d = do
    createDir d
    h <- home $ d { name = "", extension = "" }
    System.Posix.changeWorkingDirectory h

perm :: String -> FilePath -> IO ()
perm flags f = do
    system $ unwords [ "chmod",  flags, f ]    
    return ()


lesen :: Datei -> IO(String)
lesen d  = do
    h <- home d
    readFile h

existiert :: Datei -> IO Bool
existiert d  = do
    h <- home d
    doesFileExist h

dirlesen :: Datei -> IO [String]
dirlesen d  = do
    h <- home d
    getDirectoryContents h

----------------------------------------------------------------------------

erzeugeVerzeichnisse :: Datei -> IO ()
erzeugeVerzeichnisse = createDir

createDir :: Datei -> IO ()
createDir d = do
    sanity d
    h <- home_dir
    sequence_ $ do
        prefix <- inits $ h : pfad d
	guard $ not $ null prefix
	let path = concat $ intersperse "/" prefix
 	return $ do 
		ok <- doesDirectoryExist path
	    	when ( not ok ) $ do 
		       createDirectory path
		           `catch` \ any ->
			       error $ unlines [ show any, show path ]
		       perm "go+rx" path 
		       return ()
