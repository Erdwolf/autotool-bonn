module Multi.Selection where

-- $Id$

import Multi.Config
import Multi.Paths

import Util.Zufall
import Util.Datei

import Monad ( guard )

data Item = 
     Item { picture :: FilePath -- ohne dir
	  , alternatives :: [ String ] -- vorschläge
	  , solution :: Int -- der isses (zähl. beginnt bei 0)
	  }

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
	 return $ Item { picture = pic 
		       , alternatives = alts
		       , solution = head 
			     $ do i <- [0..] ; guard $ 0 == p !! i ; return i
		       }
	 ) pick

