module Grammatik.CF.Interface where

-- -- $Id$

import Language.Type
import Grammatik.Type

import qualified Reporter.Checker as C

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
	   [ text "Gesucht ist eine kontextfreie Grammatik für die Sprache"
	   , nest 4 $ text $ abbreviation $ I.lang i
	   , text "über dem Alphabet" <+> toDoc ( alphabet $ I.lang i )
	   , text ""
	   , text "Zu dieser Sprache gehören unter anderem die Wörter:"
	   , nest 4 $ toDoc $ take 10 $ I.yeah i
	   , text ""
	   , C.condition $ I.typ i
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
          C.run typ2 b

    total p i b = do
          cf_yeah_noh i b
          C.run ( I.typ i ) b

---------------------------------------------------------------------------

make :: P.Config -> IO Variant
make = return . Variant . make0

make0 :: P.Config 
	 -> Var CFG I.Config Grammatik
make0 c = Var { problem = CFG
        , aufgabe = C.nametag ( P.typ c )
        , version = nametag ( P.lang c )
        , key = \ matrikel -> do 
          return matrikel
        , gen = \ key -> do
          seed $ read key

          let l = P.lang c ; w = P.min_sample_length c ; n = P.num_samples c
          -- die kleinen sollten ja auch schnell zu testen sein
	  let klein = take 40 $ do 
		 n <- [0 .. ]
		 alle ( setToList $ alphabet l ) n
		 
          -- watch argument ordering! -- TODO: use records instead
          here   <- samples      l n w
          there  <- anti_samples l n w
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









