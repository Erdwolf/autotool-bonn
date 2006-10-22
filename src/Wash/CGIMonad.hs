-- © 2001, 2002 Peter Thiemann

-- with modifications by Johannes Waldmann joe@informatik.uni-leipzig.de
--   $Id$

module Wash.CGIMonad
where

import Prelude hiding ( span, div, head, map )

import Wash.RawCGI
import Wash.HTMLMonad

-- 
newtype CGI a = CGI { unCGI :: CGIState -> IO (a, CGIState) }
data CGIState
  = CGIState { inparm   :: [PARAMETER]
	     , outparm  :: [PARAMETER]
	     , cgiInfo  :: CGIInfo
	     , pageInfo :: PageInfo
	     , encoder  :: String -> String
	     , cookieMap     :: [(String, (Maybe String, Maybe String))]
	     , cookiesToSend :: [String]
	     }

data PageInfo =
     PageInfo { count      :: Int
     	      , vcount     :: Int
     	      , nextaction :: Element -> CGI ()
	      , bindings   :: Maybe CGIParameters
	      , enctype    :: String
	      , faultyfields :: [String]
	      }

url = cgiUrl . cgiInfo
contentType = cgiContentType . cgiInfo

fromCGIstate select =
  CGI $ \cgistate ->
  return (select cgistate, cgistate)

getUrl = fromCGIstate url
getParm = fromCGIstate outparm
getInfo = fromCGIstate pageInfo
getEncoder = fromCGIstate encoder
getPathInfo = fromCGIstate (cgiPathInfo . cgiInfo)

inc = 
  CGI $ \cgistate ->
  let info = pageInfo cgistate in
  return (info 
         ,cgistate { pageInfo = info { count = count info + 1}})

vinc = 
  CGI $ \cgistate ->
  let info = pageInfo cgistate in
  return (info 
         ,cgistate { pageInfo = info { vcount = vcount info + 1}})

setAction :: (Element -> CGI ()) -> CGI ()
setAction actionFun =
  CGI $ \cgistate ->
  return (()
         ,cgistate { pageInfo = (pageInfo cgistate) { nextaction = actionFun }})

setEnctype :: String -> CGI ()
setEnctype contentType =
  CGI $ \cgistate ->
  return (()
         ,cgistate { pageInfo = (pageInfo cgistate) { enctype = contentType } })

setFaulty :: [String] -> CGI ()
setFaulty ss =
  CGI $ \cgistate ->
  return (()
         ,cgistate { pageInfo = (pageInfo cgistate) { faultyfields = ss } })

data PARAMETER 
	= PAR_RESULT String
	| PAR_VALUES CGIParameters
	| PAR_MARK
	deriving (Show, Read)

instance Monad CGI where
  return a = 
	CGI ( \cgistate -> return (a, cgistate))
  CGI cgi >>= f = 
	CGI ( \cgistate -> 
	    cgi cgistate >>= \ (x, cgistate') ->
	    unCGI (f x) cgistate')

{-- 
fixCGI :: (a -> CGI a) -> CGI a
fixCGI f = CGI (\cgistate -> fixIO (\as' -> unCGI (f (fst as')) cgistate))
--}

nextName :: CGI String
nextName =
  do pageInfo <- inc
     return ('f' : show (count pageInfo))

nextValue :: CGI String
nextValue =
  do pageInfo <- vinc
     return ('v' : show (vcount pageInfo))

