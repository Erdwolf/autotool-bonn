module Inter.Store where

--  $Id$

import Util.Datei
import qualified System.Posix
import qualified Inter.Param as P

import Control.Types (toString, fromCGI, File, Wert(..))
import Control.Monad ( when )
import Inter.Logged

import Data.Maybe
import Data.Char

data Type = Instant | Input | Report deriving ( Eq, Ord, Show )

-- | alles: speichert in "latest.input"
-- d. h. überschreibt immer
-- zur sicherheit auch: von richtigen einsendungen: speicher in "$pid.input"
-- d. h. eigentlich kein überschreiben
store :: Type -> P.Type -> IO ( String, Maybe File )
store ty p = logged "Inter.store" $ do
    pid <- fmap show $ System.Posix.getProcessID 
    let flag = case P.mresult p of Just (Ok _) -> True ; _ -> False
        mthing = case ty of 
                   Input -> P.input p
                   Instant -> fmap show $ P.minstant p
                   Report -> fmap show $  P.report p
    when flag $ do
            logged "Inter.store.schreiben" 
               $ mschreiben ( location ty p pid flag ) $ mthing
            return ()
    f <- mschreiben ( location ty p "latest" flag ) $ mthing
    return ( pid , fmap fromCGI f )

latest :: Type -> P.Type -> IO String
latest ty p = logged "Inter.latest" $ do
    lesen ( location ty p "latest" False ) 

load :: Type -> P.Type -> String -> Bool -> IO String
load ty p pid flag = logged "Inter.load" $ do
    lesen ( location ty p pid flag )
    

location :: Type -> P.Type -> String -> Bool -> Datei
location ty p pid flag =  
    Datei { pfad = [ "autotool", "done"
			    , toString $ P.vnr p
		            , toString $ P.anr p
			    , P.smatrikel p
			    , if flag then "OK" else "NO"
			    ]
                   , name = pid
		   , extension = map toLower $ show ty
		   } 