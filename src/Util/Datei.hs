{-# OPTIONS -cpp #-}

module Autolib.Util.Datei 

-- -- $Id$

 (  schreiben
  , anhaengen
  , lesen
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

-- alle pfade sind relativ zu $HOME, falls das existiert
-- in CGI-skripten existiert es nicht (?)
-- dann relativ zu /home/$(posix.getloginname)

-- �nderung: das CGI-skript mu� die env-var $HOME setzen
-- damit wir hier nicht vom modul Posix anh�ngen,
-- das es in hugs n�mlich gar nicht gibt.
-- CGI ist ja immer ghc-compiled,
-- aber bei klassischen aufgaben l�uft hier hugs

data Datei = Datei{ pfad :: [String]
                  , name :: String
                  , extension :: String
                  }

instance Show Datei where
    show d = "$HOME/" ++ inner d

namex d = name d 
       ++ if null (extension d) then "" else "." ++ extension d

-- ohne $HOME-prefix
inner :: Datei -> String 
inner d =  concat 
	$ intersperse "/" 
	$ pfad d ++ [ namex d ]

outer :: Datei -> String
-- das kann als arg. f�r CGI verwendet werden:
outer d =  concat 
	$ intersperse "," 
	$ pfad d ++ [namex d ]
-- dann darf im filenamen nat�rlich kein komma stehen


    
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
#ifdef SPACE
	    return "/space"
#else
#ifdef HOME
	    getEnv "HOME"
#else
#ifdef POSIX
        user <- System.Posix.getEffectiveUserName
        return $ "/home/" ++ user
#else            
        let user = "autotool"
        return $ "/home/" ++ user
#endif
#endif
#endif

sanity :: Datei -> IO ()
-- mit gro�er vorsicht: �ber das argument von Get.cgi
-- k�nnte jemand von au�en files zu lesen probieren.
-- inklusive aller tricks mit "../.." usw.
-- deswegen vor jedem zugriff testen
sanity d = 
    when ( not $ all isAlphaNum
	       $ concat $ pfad d ++ [ name d , extension d ] 
	 ) $ error "strange zeichen in dateiname"


home :: Datei -> IO FilePath
home d = do
    sanity d
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
