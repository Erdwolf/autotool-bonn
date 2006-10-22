module Multi.Selection where

--   $Id$

import Multi.Config
import Multi.Paths

import Util.Zufall
import Util.Datei

import Types -- wash
import qualified Posix

import Control.Monad ( guard )

data Item = 
     Item { picture :: FilePath -- ohne dir
	  , alternatives :: [ String ] -- vorschläge
	  , solution :: Int -- der isses (zähl. beginnt bei 0)
	  }
     deriving ( Read, Show )

instance Types Item where
  ty it = TS (TRData (tdName itemDef) [tra, trb, trc]) 
  	     (foldr1 merge [ defsa , defsb , defsc , [itemDef]] )
    where TS tra defsa = ty (picture      it)
	  TS trb defsb = ty (alternatives it)
	  TS trc defsc = ty (solution     it)

itemDef = TD "Item" [] [CR "Item"  Nothing [] ]



selection :: Config -> IO [ Item ]
selection conf = do
    index <- lesen $ data_index $ prefix conf 
    let obnames = do
	    line <- lines index
	    -- vor leerzeichen steht (rel.) filename, danach beschreibung
	    return $ span (/= ' ') line	
    let nams = map snd obnames -- alle

    pick <- einige ( objects conf ) obnames 
    mapM ( \ (pic, correct) -> do
	 others <- einige ( names conf - 1 ) nams
	 let suggs = correct : others
	 p <- permutation [ 0 .. length suggs - 1 ]
	 let alts = do i <- p ; return $ suggs !! i
	 user <- Posix.getEffectiveUserName
	 let loc = ( prefix conf ) 
	           { name = pic
		   , pfad = [ "", "~" ++ user ] ++ pfad ( prefix conf )
		   }
	 return $ Item { picture = inner loc
		       , alternatives = alts
		       , solution = head 
			     $ do i <- [0..] ; guard $ 0 == p !! i ; return i
		       }
	 ) pick

