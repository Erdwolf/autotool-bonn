module Robots.Konfig 

( Konfig -- abstract
, mkKonfig, geschichte
, move, remove, addZug
, look, robots
, positions, goals
, valid

)

where

-- -- $Id$

import Robots.Data

import List (partition)
import Monad ( guard )
import Maybe ( isJust, maybeToList, fromMaybe )
import FiniteMap
import Sets


import ToDoc
import Reporter



data Konfig = Konfig { inhalt :: FiniteMap String Robot
		     , geschichte :: [ Zug ]
		     }


mkKonfig :: [ Robot ] -> Konfig
mkKonfig rs = Konfig { inhalt = listToFM $ do r <- rs ; return ( name r, r )
		     , geschichte = []
		     }


-- instance Number Konfig Konfig where number = id
-- instance Iso Konfig where iso = (==)



remove :: String -> Konfig -> Konfig
-- fort damit (into outer space)
remove n k = k { inhalt = delFromFM (inhalt k) n
		 }

move :: (String, Position) -> Konfig -> Konfig
-- auf neue position
move (n, p) k = 
    let i = inhalt k
    in  k { inhalt = addToFM i n 
	      $ let r = fromMaybe ( error "Robots.Move.move" ) ( lookupFM i n )
		in  r { position = p }
	  }

addZug :: Zug -> Konfig -> Konfig
addZug z k = k { geschichte = z : geschichte k }

instance ToDoc Konfig where
    toDoc k = text "mkKonfig" <+> toDoc ( robots k )

instance Show Konfig where show = render . toDoc

instance Read Konfig where
    readsPrec p cs = do
        ( "mkKonfig", cs ) <- lex cs
        ( arg, cs ) <- reads cs
        return (mkKonfig arg, cs)

essence :: Konfig -> ( Set Robot, Set Position )
-- die mit ziel werden echt verglichen,
-- von den anderen nur die positionen
essence k = 
    let rs = robots k
	(zs, ns) = partition ( isJust . ziel ) rs
    in	(mkSet zs, mkSet $ map position ns)

instance Ord Konfig where
    compare k l = compare (essence k) (essence l)
instance Eq Konfig where
    (==) k l = (==)  (essence k) (essence l)

----------------------------------------------------------------------

look :: Konfig -> String -> Maybe Robot
look (Konfig k g) n = lookupFM k n

robots :: Konfig -> [ Robot ]
robots (Konfig k g) = eltsFM k 

positions :: Konfig -> [ Position ]
positions = map position . robots

goals :: Konfig -> [ Position ]
goals k = do r <- robots k ; maybeToList ( ziel r )

valid :: Konfig -> Reporter ()
valid k = do
    let mappe = addListToFM_C (++) emptyFM $ do
	      r <- robots k
	      return ( position r, [ name r ] )
    let mehrfach = do 
	      ( p, rs ) <- fmToList mappe
	      guard $ length rs > 1
	      return ( p , rs )
    inform $ text "Stehen alle Roboter auf verschiedenen Positionen?"
    if ( null mehrfach )
	   then inform $ text "Ja."
	   else reject $ text "Nein, diese nicht:" <+> toDoc mehrfach 




