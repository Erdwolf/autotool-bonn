module Grammatik.CF.Check where

-- $Id$

import Language.Type
import Grammatik.Type

import qualified Grammatik.CF.Instance.Config as I
import qualified Grammatik.CF.Problem.Config as P

import Grammatik.CF.Yeah_Noh

import Grammatik.Hierarchie ( typ2 )

import Sets

import Util.Seed
import Util.Wort
import Util.Zufall

import Inter.Types
import Challenger.Partial
import Reporter
import ToDoc

import List (partition, nub)

data CFG = CFG deriving ( Eq, Ord, Show, Read )

instance Partial CFG I.Config Grammatik where
    describe p i = vcat
	   [ text "Gesucht ist eine kontextfreie Grammatik f�r die Sprache"
	   , nest 4 $ text $ abbreviation $ I.lang i
	   , text "�ber dem Alphabet" <+> toDoc ( alphabet $ I.lang i )
	   ]
    initial  p i = Grammatik 
	   { terminale = alphabet $ I.lang i
	   , nichtterminale = mkSet "ST"
	   , startsymbol = 'S'
	   , regeln = mkSet $
                 let a : b : rest = setToList $ alphabet $ I.lang i
	         in  [ ( "S", [ 'T', a, 'S' ] ) , ("S", ""), ( "T", [ b, b ] ) ]
	   }

    partial p i b = do
          typ2 b
          I.typ i b

    total p i b = do
          cf_yeah_noh i b

---------------------------------------------------------------------------

make :: P.Config -> IO Variant
make = return . Variant . make0

make0 :: P.Config 
	 -> Var CFG I.Config Grammatik
make0 c = Var { problem = CFG
        , aufgabe = "G"
        , version = nametag ( P.lang c )
        , key = \ matrikel -> do 
          return matrikel
        , gen = \ key -> do
          seed $ read key

          let l = P.lang c ; w = P.min_sample_length c ; n = P.num_samples c
          -- die kleinen sollten ja auch schnell zu testen sein
	  let klein = take 100 $ do 
		 n <- [0 .. ]
		 alle ( setToList $ alphabet l ) n
		 
          here   <- samples      l w n
          there  <- anti_samples l w n
          let (yeahs, nohs) = partition (contains l) 
			    $ nub 
			    $ klein ++ here ++ there

          return $ return $ I.Config 
		 { I.lang = P.lang c
		 , I.typ  = P.typ  c
		 , I.yeah = yeahs
		 , I.noh  = nohs
                 }
        }









