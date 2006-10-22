module Grammatik.CF.Language where

--  $Id$

import Language.Type
import Grammatik.Type

import Grammatik.CF.Akzeptor
import qualified Grammatik.CF.Chomsky as C
import Grammatik.CF.Create
import Util.Zufall
import Util.Uniq
import ToDoc
import Edit

make :: String -- ^ nametag
     -> Grammatik 
     -> Language
make nam g = 
     let 
	 l = Language 
	   { abbreviation = show $ text "L (G) für G = " <+> toDoc g
	   , nametag = nam
	   , alphabet = terminale g
	   , contains = akzeptor g 
	   , sample = \ c n -> do
	       let ws = take (c * c) $ create g (n+1)
	       if null ws 
	          then return []
	   	  else do
	   	     us <- sequence $ replicate c $ eins ws
	   	     return $ uniq us
	   , anti_sample = \ c n -> do
	        ws <- sample l c n
		us <- mapM edits $ concat $ replicate 1 ws
		return $ take c $ filter ( not . contains l ) us
	   }
     in  l

ex :: Grammatik
ex = Grammatik
   { terminale = mkSet "ab"
   , nichtterminale = mkSet "S"
   , startsymbol = 'S'
   , regeln = mkSet [ ("S", ""), ("S", "aSbS") ]
   }
