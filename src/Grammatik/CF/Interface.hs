module Grammatik.CF.Interface where

-- -- $Id$


import Language.Type
import Language.Syntax
import Language.Inter
import Grammatik.Type

import qualified Grammatik.CF.Instance.Config as I
import qualified Grammatik.CF.Problem.Config as P

import Grammatik.CF.Yeah_Noh

import Grammatik.Property

import Autolib.Set
import Autolib.Util.Wort


import Inter.Types
import Inter.Quiz
import Challenger.Partial
import Autolib.Reporter
import Autolib.ToDoc

import Data.List ( partition, nub )
import Data.Typeable

data CFG = CFG deriving ( Eq, Ord, Show, Read, Typeable )

instance Partial CFG I.Config Grammatik where

    describe p i = vcat
	   [ text "Gesucht ist eine kontextfreie Grammatik für die Sprache"
           , nest 4 $ toDoc $ inter $ I.lang i
	   , text "über dem Alphabet" <+> toDoc ( alphabet $ inter $ I.lang i )
	   , text ""
	   , text "Zu dieser Sprache gehören unter anderem die Wörter:"
	   , nest 4 $ toDoc $ take 10 $ I.unLong $ I.yeah i
	   , text ""
	   , text "Die Grammatik soll diese Eigenschaften haben:"
           , nest 4 $ toDoc $ I.properties i
	   ]

    initial  p i = Grammatik 
	   { terminale = alphabet $ inter $ I.lang i
	   , variablen = mkSet "ST"
	   , start = 'S'
	   , regeln = mkSet $
                 let a : b : rest = setToList $ alphabet $ inter $ I.lang i
	         in  [ ( "S", [ 'T', a, 'S' ] ) , ("S", ""), ( "T", [ b, b ] ) ]
	   }

    partial p i b = do
          Grammatik.Property.check ( Typ 2 ) b

    total p i b = do
          cf_yeah_noh i b
          mapM_ ( \ p -> Grammatik.Property.check p b ) ( I.properties i )

---------------------------------------------------------------------------

instance Project CFG I.Config I.Config where
    project _ i = i

instance Generator CFG P.Config I.Config where
    generator _ c key = do
          let l = inter $ P.lang c 
              w = P.min_sample_length c 
              n = P.num_samples c
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

          return $ I.Config 
		 { I.lang = P.lang c
		 , I.properties  = P.properties c
		 , I.yeah = I.Long yeahs
		 , I.noh  = I.Long nohs
                 }

make :: Make
make = quiz CFG P.example










