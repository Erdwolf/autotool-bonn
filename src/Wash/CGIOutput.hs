-- © 2001, 2002 Peter Thiemann

-- with modifications by Johannes Waldmann joe@informatik.uni-leipzig.de
--   $Id$

module Wash.CGIOutput where

import Prelude hiding ( span, div, head, map )

import Maybe
import IO
import System

import Wash.Config
import Wash.HTMLWrapper
import Wash.RawCGI (URL, FileReference (..))

itell :: WithHTML IO () -> IO a
itell hma = do elem <- build_document hma
	       cgiPut elem
	       exitWith ExitSuccess

class CGIOutput a where
  cgiPut :: a -> IO ()
  cgiPutList :: [a] -> IO ()
  
  cgiPutList xs = cgiPutList xs

instance CGIOutput a => CGIOutput [a] where
  cgiPut xs = cgiPutList xs

-- instance CGIOutput Element where
instance CGIOutput ELEMENT_ where
  cgiPut elem =
    do putStrLn "Content-Type: text/html"
       putStrLn ""
       putStr $ show elem

-- 
instance CGIOutput Char where
  cgiPut ch = cgiPutList [ch]
  cgiPutList str =
    do putStrLn "Content-Type: text/plain"
       putStrLn ""
       putStr str
-- 
instance CGIOutput FileReference where
  cgiPut fr =
    do putStr "Content-Type: "
       putStrLn (fileReferenceContentType fr)
       putStrLn ""
       hFlush stdout
       system (catProgram ++ " " ++ fileReferenceName fr)
       return ()
--       contents <- readFile (fileReferenceName fr)
--       putStr contents

-- 
data Status = Status Int				    -- status code
		     String				    -- reason phrase
		     (Maybe (WithHTML IO ()))		    -- more explanation

instance CGIOutput Status where
  cgiPut (Status status reason_phrase elems) =
    do putStr "Status: "
       putStr status_str
       putStr " "
       putStrLn reason_phrase
       case elems of
         Nothing -> putStrLn ""
	 Just _  -> itell message
    where status_str = show status
	  ttl = "Error: " ++ status_str ++ ' ' : reason_phrase
	  message = standardPage ttl (fromJust elems)
	  
-- 	
newtype Location = Location URL

instance CGIOutput Location where
  cgiPut (Location url) =
    do putStr "Location: "
       putStrLn url
       putStrLn ""

-- 
data FreeForm = FreeForm String String String

instance CGIOutput FreeForm where
  cgiPut (FreeForm fileName contentType rawContents) =
    do putStr "Content-Type: "
       putStrLn contentType
       putStrLn ""
       putStr rawContents
