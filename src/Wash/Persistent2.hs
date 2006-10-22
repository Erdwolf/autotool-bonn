-- © 2001, 2002 Peter Thiemann

-- with modifications by Johannes Waldmann joe@informatik.uni-leipzig.de
--   $Id$

module Wash.Persistent2 (T (), init, get, set, add, current) where

import Prelude hiding ( init, span, div,  map )

import Wash.Config

{- glossary

A persistent entity (PE) is a named global value that evolves over time.
The current value of a PE is the value that the PE has now.
A handle gives access to a snapshot of a persistent entity at a particular time.

-- interface

data T a 
  type of handles to a PE of type a

init name initialValue
  creates a new PE with named name with initial value initialValue and returns the  
  handle to the initial value. 
  If the PE already exists, then init returns the handle to the current value.

get handle 
  retrieves the value from the handle. This value may not be current because the
  handle may point to a snapshot from the past.
  
set handle newValue
  tries to overwrite the value of the pe pointed to by handle with newValue. 
  Succeeds (Just handle') if the handle is a handle to the current value, 
  in this case it returns a handle to the new value.
  Fails (Nothing) if the handle is not current.

add handle addValue
  handle must point to a value of type [a]
  Prepends the addValue to the current value and returns a handle to the 
  updated value.

current handle
  returns the current handle to the PE pointed to by handle.

-}

import Wash.CGI hiding (head)
import System

import qualified Prelude (init)
import Data.List (find)
import qualified List (init)
import Maybe
import IO
import Directory
import Control.Monad
import Types
import qualified Locking as L

import qualified Exception

-- 
init    :: (Read a, Show a, Types a) => String -> a -> CGI (Maybe (T a))
get     :: Read a => T a -> CGI a
set     :: (Read a, Show a) => T a -> a -> CGI (Maybe (T a))
add     :: (Read a, Show a) => T [a] -> a -> CGI (T [a])
current :: Read a => T a -> CGI (T a)
-- 

data T a = T String Int 
  deriving (Read, Show)

data P a = P { nr :: Int, vl :: a }
  deriving (Read, Show)

t :: String -> P a -> T a
t name (P i a) = T name i

trace s = do
  -- appendFile (persistent2Dir ++ "TRACE") (s ++ "\n")
  return ()

init name val = do
  unsafe_io $
     catch (createDirectory (List.init persistent2Dir))
           (\ioe -> if isAlreadyExistsError ioe
	            then return ()
		    else ioError ioe)
  io $ Exception.catch (
     do trace ("P2: init " ++  name ++ " " ++ show val)
	obtainLock name
	trace ("P2.init: after obtainLock ")
	contents <- readFileStrictly fileName
	trace ("P2.init: after readFileStrictly " ++ contents)
	releaseLock name
	trace ("P2.init: after releaseLock[1] " ++ contents)
	(tyspec, pairs) <- return $ read contents
	trace ("P2.init: after read contents " ++ contents)
	-- let (tyspec, pairs) = read contents
	-- CHANGED BECAUSE OF NHC98 BUG
	if tyspec == myTyspec
	    then return $ Just $ t name $ head $ pairs
	    else return Nothing)
     $ \ ioError ->
     do trace ("P2.init: ioError caught")
	writeFile fileName (show (myTyspec, [initialP]))
	trace ("P2.init: writing " ++ show (myTyspec, [initialP]))
	releaseLock name
	let thing = (Just (t name initialP))
	trace ("P2.init: returning " ++ show thing )
	return thing
  where
    fileName = persistent2Dir ++ name
    myTyspec = ty val
    initialP = P 0 val

get (T name i) =
  unsafe_io $
  do trace ("P2: get " ++ name ++ " " ++ show i)
     obtainLock name
     contents <- readFileStrictly fileName
     releaseLock name
     -- 	(tyspec, pairs) = read contents
     --      CHANGED BECAUSE OF NHC98 BUG
     (tyspec, pairs) <- return $ read contents
     let foo_ = tyspec :: TySpec
     return $ vl $ fromJust $ find ((== i) . nr) pairs
  where
    fileName = persistent2Dir ++ name

set (T name i) val =
  io $
  do trace ("P2: set " ++ name ++ " " ++ show i ++ " " ++ show val)
     obtainLock name
     contents <- readFileStrictly fileName
     (tyspec, pairs) <- return $ read contents
     let foo_ = tyspec :: TySpec
	 -- (tyspec, pairs) = read contents
	 -- CHANGED BECAUSE OF NHC98 BUG
	 i' = i + 1
     if nr (head pairs) == i
       then do writeFile fileName (show (tyspec, P i' val : pairs))
	       releaseLock name
	       return (Just (T name i'))
       else do releaseLock name
	       return Nothing
  where
    fileName = persistent2Dir ++ name

add (T name _) val =
  io $
  do trace ("P2: add " ++ name ++ " " ++ show val)
     obtainLock name
     contents <- readFileStrictly fileName
     (tyspec, pairs) <- return $ read contents
     let foo_ = tyspec :: TySpec
	 -- (tyspec, pairs) = read contents
	 -- CHANGED BECAUSE OF NHC98 BUG
	 P i vals = head pairs
	 i' = i + 1
     writeFile fileName (show (tyspec, P i' (val : vals) : pairs))
     releaseLock name
     return (T name i')
  where
    fileName = persistent2Dir ++ name

current (T name _) =
  io $
  do trace ("P2: current " ++ name)
     obtainLock name
     contents <- readFileStrictly fileName
     releaseLock name
     (tyspec, pairs) <- return $ read contents
     let foo_ = tyspec :: TySpec
	 -- (tyspec, pairs) = read contents
	 -- CHANGED BECAUSE OF NHC98 BUG
	 P i vals = head pairs
     return (t name (head pairs))
  where
    fileName = persistent2Dir ++ name

readFileStrictly filePath =
  do trace ("P2.readFileStrictly: before open")
     h <- openFile filePath ReadMode
     trace ("P2.readFileStrictly: open succeeded")
     contents <- hGetContents h
     hClose (g contents h)
     return contents
  where
    g [] h = h
    g (_:rest) h = g rest h

-- 

obtainLock  name = L.obtainLock (persistent2Dir ++ name)
releaseLock name = L.releaseLock (persistent2Dir ++ name)


