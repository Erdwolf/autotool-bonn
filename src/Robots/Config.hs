module Robots.Config 

( Config -- abstract
, make, geschichte
, move, remove, addZug
, look, robots
, positions, goals
, valid

)

where

--  $Id$

import Robots.Data

import Autolib.FiniteMap
import Autolib.Set

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Xml
import Autolib.Reporter

import Data.List (partition)
import Control.Monad ( guard )
import Data.Maybe ( isJust, maybeToList, fromMaybe )
import Data.Typeable


data Config = Config { inhalt :: FiniteMap String Robot
		     , geschichte :: [ Zug ]
		     }
     deriving ( Typeable )

make :: [ Robot ] -> Config
make rs = Config 
	{ inhalt = listToFM $ do 
	      r <- rs
	      return ( name r, r )
	, geschichte = []
	}

-- | fort damit (into outer space)
remove :: String -> Config -> Config
remove n k = k { inhalt = delFromFM (inhalt k) n
		 }
-- | auf neue position
move :: (String, Position) -> Config -> Config
move (n, p) k = 
    let i = inhalt k
    in  k { inhalt = addToFM i n 
	      $ let r = fromMaybe ( error "Robots.Move.move" ) ( lookupFM i n )
		in  r { position = p }
	  }

addZug :: Zug -> Config -> Config
addZug z k = k { geschichte = z : geschichte k }

instance ToDoc Config where
    toDoc k = text "make" <+> toDoc ( robots k )

instance Reader Config where
    reader = do
        my_reserved "make"
        arg <- reader
        return $ make arg

instance Container Config [ Robot ] where
    label _ = "Config"
    pack = robots
    unpack = make

-- | die mit ziel werden echt verglichen,
-- | von den anderen nur die positionen
essence :: Config -> ( Set Robot, Set Position )
essence k = 
    let rs = robots k
	(zs, ns) = partition ( isJust . ziel ) rs
    in	(mkSet zs, mkSet $ map position ns)

instance Ord Config where
    compare k l = compare (essence k) (essence l)
instance Eq Config where
    (==) k l = (==)  (essence k) (essence l)

-----------------------------------------------------------------

look :: Config -> String -> Maybe Robot
look (Config k g) n = lookupFM k n

robots :: Config -> [ Robot ]
robots (Config k g) = eltsFM k 

positions :: Config -> [ Position ]
positions = map position . robots

goals :: Config -> [ Position ]
goals k = do r <- robots k ; maybeToList ( ziel r )

valid :: Config -> Reporter ()
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




