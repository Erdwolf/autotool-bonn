module Wash.Persistent (T (), init, get, set, add, current) where

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
import Prelude hiding (init)
import Data.List hiding (init)
import Maybe
import IO
import Control.Monad
-- 
init    :: (Read a, Show a) => String -> a -> CGI (T a)
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

trace s = 
  -- appendFile (persistentDir ++ "TRACE") (s ++ "\n")
  return ()

init name val =
  io $ catch (
  do trace ("init " ++  name ++ " " ++ show val)
     contents <- readFileStrictly fileName
     let pairs = read contents
     return $ t name $ head $ pairs)
  $ \ ioError ->
  let p = P 0 val in
  do writeFile fileName (show [p])
     return (t name p)
  where
    fileName = persistentDir ++ name

get (T name i) =
  unsafe_io $
  do trace ("get " ++ name ++ " " ++ show i)
     contents <- readFileStrictly fileName
     return $ vl $ fromJust $ find ((== i) . nr) (read contents)
  where
    fileName = persistentDir ++ name

set (T name i) val =
  io $
  do trace ("set " ++ name ++ " " ++ show i ++ " " ++ show val)
     contents <- readFileStrictly fileName
     let pairs = read contents
	 i' = i + 1
     if nr (head pairs) == i
       then do writeFile fileName (show (P i' val : pairs))
	       return (Just (T name i'))
       else return Nothing
  where
    fileName = persistentDir ++ name

add (T name _) val =
  io $
  do trace ("add " ++ name ++ " " ++ show val)
     contents <- readFileStrictly fileName
     let pairs = read contents
	 P i vals = head pairs
	 i' = i + 1
     writeFile fileName (show (P i' (val : vals) : pairs))
     return (T name i')
  where
    fileName = persistentDir ++ name

current (T name _) =
  io $
  do trace ("current " ++ name)
     contents <- readFileStrictly fileName
     let pairs = read contents
	 P i vals = head pairs
     return (t name (head pairs))
  where
    fileName = persistentDir ++ name

readFileStrictly filePath =
  do h <- openFile filePath ReadMode
     contents <- hGetContents h
     hClose (g contents h)
     return contents
  where
    g [] h = h
    g (_:rest) h = g rest h
