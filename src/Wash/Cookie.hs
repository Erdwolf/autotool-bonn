-- © 2001, 2002 Peter Thiemann

-- with modifications by Johannes Waldmann joe@informatik.uni-leipzig.de
--   $Id$

module Wash.Cookie (T, init, initExpiring, get, set, delete) where

import Wash.Locale
import Maybe
import Prelude hiding (init, div, span, head, map)
import Time

import Wash.CGIInternals
import Wash.CGIMonad
import Wash.HTMLMonad
import Wash.Types

-- 
initExpiring :: (Read a, Show a, Types a) => String -> Int -> a -> CGI (T a)
init    :: (Read a, Show a, Types a) => String -> a -> CGI (T a)
get     :: (Read a, Show a, Types a) => T a -> CGI a
set     :: (Read a, Show a, Types a) => T a -> a -> CGI (Maybe (T a))
delete  :: (Types a) => T a -> CGI ()
--current :: Read a => T a -> CGI (T a)
-- 

data T a = T String Int | Tvirtual { tvirtual :: a }
  deriving (Read, Show)

data P a = P { nr :: Int, vl :: a }
  deriving (Read, Show)

t :: String -> P a -> T a
t name (P i a) = T name i

initExpiring name minutes val =
  once $ do now <- unsafe_io getClockTime
	    let expireAt = addToClockTime TimeDiff {tdYear = 0,
						    tdMonth = 0,
						    tdDay = 0,
						    tdHour = 0,
						    tdMin = minutes,
						    tdSec = 0,
						    tdPicosec = 0} now
		-- Wdy, DD-Mon-YYYY HH:MM:SS GMT
		fmt = "%a, %d-%b-%Y %H:%M:%S GMT"
		loc = defaultTimeLocale
		timestr = formatCalendarTime loc fmt (toUTCTime expireAt)
	    unsafe_init name val (Just timestr)

init name val = 
  once (unsafe_init name val Nothing)

unsafe_init name val mexp =
  CGI (\cgistate ->
     case cookieLookup cgistate typedName of
     -- if name present and types match then construct handle from map entry
       Just pair ->
         return (t name pair, cgistate)
       Nothing -> 
     -- extend cookie map with name=val
         let p0val = P 0 val
	     cm' = (typedName, (Just (show p0val), mexp)) : cookieMap cgistate
     -- register cookie for sending
	     cts = cookiesToSend cgistate
	     cts' = if typedName `elem` cts then cts else typedName : cts
     -- construct handle
	 in return (t name p0val,
	 	    cgistate { cookieMap = cm'
		             , cookiesToSend = cts'
			     }))
  where myTyspec = ty val
	typedName = makeTypedName name myTyspec
  
get handle =
  once (unsafe_get handle)

unsafe_get t@(T name i) =
  CGI (\cgistate ->
     case cookieLookup cgistate typedName of
     -- if name present and types match then construct handle from map entry
       (Just pair) | nr pair == i ->
	 return (vl pair, cgistate)
       _ -> 
	 reportError "Cookie disappeared" 
	   (do let cm = cookieMap cgistate
		   mms = lookup typedName cm
		   mmp = if True then fmap (fmap reads . fst) mms 
		                 else Just (Just [(P 0 $ tvirtual t, "")])
	       pre $ text $ ("  " ++ typedName)
	       pre $ text $ show cm
	       pre $ text $ show mms
	       pre $ text $ show mmp)
	   cgistate)
  where myTyspec = ty (tvirtual t)
	typedName = makeTypedName name myTyspec

set handle val =
  once (unsafe_set handle val)

unsafe_set (T name i) val =
  CGI (\cgistate ->
    case cookieLookup cgistate typedName of
      (Just pair) | nr pair == i ->
        let pair' = if True then P (i + 1) val else pair    -- to unify their types
	    cts = cookiesToSend cgistate
	    cts' = if typedName `elem` cts then cts else typedName : cts
	    cm' = (typedName, (Just (show pair'), Nothing)) : cookieMap cgistate
	in return (Just (t name pair'),
	              cgistate { cookieMap = cm'
		   	       , cookiesToSend = cts'
			       })
      _ -> return (Nothing, cgistate))
  where myTyspec = ty val
	typedName = makeTypedName name myTyspec

delete t = 
  once (unsafe_delete t)

unsafe_delete t@(T name i) =
  CGI (\cgistate ->
       let  cts = cookiesToSend cgistate
	    cts' = if typedName `elem` cts then cts else typedName : cts
       in return ( ()
	         , cgistate { cookieMap = (typedName, (Nothing, Nothing))
		 			  : cookieMap cgistate
		            , cookiesToSend = cts'
			    }))
  where myTyspec = ty (tvirtual t)
	typedName = makeTypedName name myTyspec

cookieLookup :: (Show a, Read a) => CGIState -> String -> Maybe (P a)
cookieLookup cgistate typedName =
  let cm = cookieMap cgistate
      fn = lookup typedName cm		    -- Maybe (Maybe String, Maybe String)
      cn = fmap (fmap reads . fst) fn	    -- Maybe (Maybe [(P a, String)])
      checkType [] = 
        Nothing
      checkType ((pair,_):_) =
        Just pair
  in  cn >>= \mtp -> mtp >>= checkType

makeTypedName :: String -> TySpec -> String
makeTypedName s tys = s ++ '-' : show tys
