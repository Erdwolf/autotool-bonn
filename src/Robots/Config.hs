module Robots.Konfig where

-- $Id$

import Robots.Data

import List (partition)
import Monad ( guard )
import Maybe ( isJust )
import FiniteMap
import ToDoc
import Boc

data Konfig = Konfig ( FiniteMap String Robot )


mkKonfig :: [ Robot ] -> Konfig
mkKonfig rs = Konfig $ listToFM $ do r <- rs ; return ( name r, r )

instance ToDoc Konfig where
    toDoc k = text "mkKonfig" <+> toDoc ( robots k )

instance Show Konfig where show = render . toDoc

instance Read Konfig where
    readsPrec p cs = do
        ( "mkKonfig", cs ) <- lex cs
        ( arg, cs ) <- reads cs
        return (mkKonfig arg, cs)

look :: Konfig -> String -> Maybe Robot
look (Konfig k) n = lookupFM k n

robots :: Konfig -> [ Robot ]
robots (Konfig k) = eltsFM k 

positions :: Konfig -> [ Position ]
positions = map position . robots

valid :: Konfig -> Boc
valid k = 
    let mappe = addListToFM_C (++) emptyFM $ do
	      r <- robots k
	      return ( position r, [ name r ] )
	mehrfach = do 
	      ( p, rs ) <- fmToList mappe
	      guard $ length rs > 1
	      return ( p , rs )
    in	explain 4 ( text "Stehen alle Roboter auf verschiedenen Positionen?" )
	$ if null mehrfach then ( True, text "Ja." )
	  else ( False, text "Nein, diese nicht:" <+> toDoc mehrfach )

final :: Konfig -> Boc
final k = 
    let robs = do r <- robots k
		  guard $ isJust $ ziel r
		  return ( ziel r == Just ( position r ) , r )
	( yeah, noh ) = partition fst robs
    in  explain 4 ( vcat [ text "Sind alle Roboter an ihren Zielpunkten?"
			 , text "Diese ja: " <+> toDoc ( map snd yeah )
			 , text "Diese nicht: " <+> toDoc ( map snd noh )
			 ]
		  )
	$ if null noh then ( True, text "OK" ) else ( False, text "nicht OK" )


------------------------------------------------------------------


ex :: Konfig
ex = mkKonfig
		  [ Robot { name = "A", position = (-2, 2), ziel = Nothing }
		  , Robot { name = "B", position = ( 0, 2), ziel = Nothing }
		  , Robot { name = "C", position = ( 2, 2), ziel = Nothing }
		  , Robot { name = "D", position = ( 2,-1), ziel = Nothing }
		  , Robot { name = "E", position = (-1,-2), ziel = Just (0,0) }
		  ]
