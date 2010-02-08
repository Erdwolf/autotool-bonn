module Grammatik.CF.Interface2 where

-- -- $Id$


import Language.Type
import Language.Syntax
import Language.Inter
import Language.Sampler
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

import Autolib.Reporter.Checker as C

import Inter.Types
import Challenger.Partial
import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Hash

import Data.List ( partition, nub )
import Data.Typeable

data CFG2 = CFG2 deriving ( Eq, Ord, Show, Read, Typeable )

instance Verify CFG2 I2.Config where

    verify p i = if Kontextfrei `elem` I2.properties i
                 then return ()
                 else reject $ text "property 'Kontextfrei' ist zwingend notwendig"


instance OrderScore CFG2 where
    scoringOrder _ = Increasing

instance Partial CFG2 I2.Config Grammatik where

    describe p i = vcat
	   [ text "Gesucht ist eine kontextfreie Grammatik für die Sprache"
           , nest 4 $ toDoc $ inter $ language $ I2.source i
	   , text "über dem Alphabet" 
                      <+> toDoc ( alphabet $ inter $ language $ I2.source i )
	   , text ""
	   , text "Die Grammatik soll diese Eigenschaften haben:"
           , nest 4 $ toDoc $ I2.properties i
	   ]

    initial  p i = Grammatik 
	   { terminale = alphabet $ inter $ language $ I2.source i
	   , variablen = mkSet "ST"
	   , start = 'S'
	   , regeln = mkSet $
                 let a : b : rest = setToList 
                      $ alphabet $ inter $ language $ I2.source i
	         in  [ ( "S", [ 'T', a, 'S' ] ) , ("S", ""), ( "T", [ b, b ] ) ]
	   }

    partial p i b = do
          C.run ( Grammatik.Property.check ( Typ 0 ) ) b
          mapM_ ( \ p -> C.run ( Grammatik.Property.check p ) b ) ( I2.properties i )

    total p i0 b = do
          let i = add_test_cases p i0 b
          cf_yeah_noh i b


---------------------------------------------------------------------------

add_test_cases p i b 
    = let large = maximum $ (0 : ) $ map ( length . snd ) $ rules b
          ( yeahs, nohs ) = Language.Sampler.create ( I2.source i ) 
                            ( hash b ) ( Just large )
                             
      in  I.Config
		 { I.lang = language $  I2.source i
		 , I.properties  = I2.properties i
		 , I.yeah = I.Long $ map I.Long yeahs
		 , I.noh  = I.Long $ map I.Long nohs
                 }

make :: Make
make = direct CFG2 I2.example










