module Grammatik.CF.Interface2 where

-- -- $Id$


import Language.Type
import Language.Syntax
import Language.Inter
import Grammatik.Type

import qualified Grammatik.CF.Instance.Config as I
import qualified Grammatik.CF.Instance.Config2 as I2
-- import qualified Grammatik.CF.Problem.Config as P

import Grammatik.CF.Yeah_Noh
import Grammatik.CF.Baum

import Grammatik.Property

import Autolib.Set
import Autolib.Util.Zufall
import Autolib.Util.Wort ( alle )

import Inter.Types
import Challenger.Partial
import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Hash

import Data.List ( partition, nub )
import Data.Typeable

data CFG2 = CFG2 deriving ( Eq, Ord, Show, Read, Typeable )

instance Partial CFG2 I2.Config Grammatik where

    describe p i = vcat
	   [ text "Gesucht ist eine kontextfreie Grammatik für die Sprache"
           , nest 4 $ toDoc $ inter $ I2.lang i
	   , text "über dem Alphabet" <+> toDoc ( alphabet $ inter $ I2.lang i )
	   , text ""
	   , text "Die Grammatik soll diese Eigenschaften haben:"
           , nest 4 $ toDoc $ I2.properties i
	   ]

    initial  p i = Grammatik 
	   { terminale = alphabet $ inter $ I2.lang i
	   , variablen = mkSet "ST"
	   , start = 'S'
	   , regeln = mkSet $
                 let a : b : rest = setToList $ alphabet $ inter $ I2.lang i
	         in  [ ( "S", [ 'T', a, 'S' ] ) , ("S", ""), ( "T", [ b, b ] ) ]
	   }

    partial p i b = do
          Grammatik.Property.check ( Typ 2 ) b

    total p i0 b = do
          let i = add_test_cases p i0 b
          cf_yeah_noh i b
          mapM_ ( \ p -> Grammatik.Property.check p b ) ( I.properties i )


---------------------------------------------------------------------------

add_test_cases p i b 
    = randomly ( fromIntegral $ hash b ) $ do
          let l = inter $ I2.lang i
              w = I2.min_sample_length i
              n = I2.num_samples i
          -- die kleinen sollten ja auch schnell zu testen sein
	  let klein = take 40 $ do 
		 n <- [0 .. ]
		 alle ( setToList $ alphabet l ) n
          -- watch argument ordering! -- TODO: use records instead
          here   <- samples      l n w
          there  <- anti_samples l n w
          -- größte rechte Seite
          let lrg = maximum $ (0 : ) $ map ( length . snd ) $ rules b
          let top = 5 + max lrg ( I2.max_sample_length i )
          farout <-      samples l 10 lrg
          let (yeahs, nohs) = partition (contains l) 
		    $ nub 
		    $ filter ( \ w -> length w <= top )
		    $ klein ++ here ++ there ++ farout
          return $ I.Config
		 { I.lang = I2.lang i
		 , I.properties  = I2.properties i
		 , I.yeah = I.Long $ map I.Long yeahs
		 , I.noh  = I.Long $ map I.Long nohs
                 }

make :: Make
make = direct CFG2 I2.example










