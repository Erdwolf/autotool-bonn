module Main where

--   $Id$

-- socket-interface f�r tool
-- geklaut und vereinfacht von Face.hs

-- Autoren: Johannes Waldmann, Alf Richter


-- hier sind die aufgaben drin:
import Inter.Boiler ( boiler )

import qualified Challenger
import Inter.Validate
import Inter.Evaluate
import Inter.Bank
import Inter.Types

import qualified Inter.Param as P
import qualified Inter.ID as I

import ToDoc (Doc, render, toDoc)
import Reporter
import qualified Output
import qualified Passwort

import qualified Posix
import qualified Network
import qualified Exception

import IO
import Control.Concurrent

main :: IO ()
main = do
    variants <- boiler
    Posix.installHandler Posix.sigPIPE Posix.Ignore Nothing
    sock <- Network.listenOn $ Network.PortNumber 1234
    sequence_ $ repeat $ waiter variants sock

waiter variants sock = do
    connection @ ( h , rhost, rport ) <- Network.accept sock
    forkIO ( do handler variants connection
	           `Exception.catch` \ any -> do
	                print any
	                return () 
	        hClose h
	   )
    
handler variants ( h, rhost, rport ) = do
    putStrLn $ "got call from " ++ show (rhost, rport)
    first <- hGetLine h
    let id = read first
    print id ; hFlush stdout

    let par = P.empty { P.problem = I.problem id
		       , P.aufgabe = I.aufgabe id
		       , P.version = I.version id
		       , P.matrikel = I.matrikel id
		       , P.passwort = read $ I.passwort id
		       -- , P.input = rest
		       , P.variants = variants
		       } 

    res <- validate par

    case res of
     Left _ -> do
          hPutStrLn h "oops"

     Right par -> case P.variante par of
       Variant v -> do
          k <- key  v $ P.matrikel par
          generator <- gen v k
          let gen @ ( Just i, com :: Doc ) = export generator
	  print gen ; hFlush stdout

	  hPutStrLn h $ show i -- Kommentar wird ignoriert
	  hFlush h

          block <- hGetBlock h 
	  
	  par <- return $ par { P.input = unlines $ block }

          let evl @ ( res :: Maybe Int , com :: Doc ) 
                 = export $ evaluate ( problem v ) i par
	  print evl ; hFlush stdout

	  msg <- bank par res 
	  print msg ; hFlush stdout

	  hPutStrLn h $ msg
	  hPutStrLn h $ render com

hGetBlock :: Handle -> IO [ String ]
-- bis zu leerzeile lesen
hGetBlock h = do
    l <- hGetLine h
    putStrLn $ "<< " ++ l ; hFlush stdout
    if null l 
	then return [ l ]
	else do ls <- hGetBlock h
	        return $ l : ls


