module Util.Datei 

--  $Id$

(  schreiben, mschreiben
, anhaengen
, lesen
, loeschen
, existiert
, dirlesen
, dirgehen
, Util.Datei.getModificationTime
, erzeugeVerzeichnisse
, home
, Datei (..)
, inner, outer, internalize
, perm
) 

where

import Debug
import Data.List (inits, intersperse)
import System.Directory
import System.Time
import Control.Monad (guard, when)
import System.Cmd ( system)

import System.IO 
import qualified System.IO.UTF8

import System.Environment ( getEnv )
import Data.Char (isAlphaNum)
import qualified System.Posix
import qualified System.Directory
import qualified Control.Exception as CE

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
    System.IO.UTF8.appendFile h inhalt

loeschen :: Datei -> IO ()
loeschen d  = do
    h <- home d
    removeFile h

schreiben :: Datei -> String -> IO FilePath
schreiben d inhalt = do
    createDir d
    h <- home d
    debug "before writeFile ..."
--    when ( length inhalt >= 0 ) -- force evaluation? 
--	 $ writeFile h inhalt
    f <- openFile h WriteMode 
    hSetEncoding f utf8
    hPutStr f inhalt
    hClose f

    debug "... after writeFile"
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
    Debug.system $ unwords [ "chmod",  flags, f ]    
    return ()


lesen :: Datei -> IO(String)
lesen d  = do
    h <- home d
    ex <- System.Directory.doesFileExist h
    if ex then do
            f <- openFile h ReadMode
            hSetEncoding f utf8
            cs <- hGetContents f
            -- hGetContents is lazy - force its result before closing the file
            CE.evaluate (last cs)
            hClose f
            return cs
        else error $ "file: " ++ h ++ " does not exist"

existiert :: Datei -> IO Bool
existiert d  = do
    h <- home d
    doesFileExist h

dirlesen :: Datei -> IO [String]
dirlesen d  = do
    h <- home d
    getDirectoryContents h

getModificationTime :: Datei -> IO ClockTime
getModificationTime d = do
    h <- home d
    System.Directory.getModificationTime h

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
		           `CE.catch` \ (CE.SomeException any) ->
			       error $ unlines [ show any, show path ]
		       perm "go+rx" path 
		       return ()
