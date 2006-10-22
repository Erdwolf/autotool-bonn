{-# OPTIONS -fallow-overlapping-instances -fglasgow-exts -fallow-undecidable-instances #-} 

module Number.Float.Config where

import Prelude hiding ( exponent )

import qualified Number.Base.Data as B

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size
import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

import Autolib.Reporter
import qualified Number.Float.Data as F

data Config = Config
	    { basis :: Int
            -- mÃ¼ssen alle normalisiert sein
	    -- , normalisiert :: Bool   
	    , max_stellen_mantisse :: Int 
	    , max_stellen_exponent :: Int 
	    }
    deriving ( Typeable )

{-! for Config derive: Reader, ToDoc, Haskell2Xml !-}

example :: Config
example = Config
        { basis = 2
        -- , normalisiert = True
        , max_stellen_mantisse = 3
        , max_stellen_exponent = 3
        }

conforms :: Config -> F.Zahl -> Reporter ()
conforms c z = do
    assert ( basis c == F.basis z )
	   $ text "Ist die Basis gleich" <+> toDoc (basis c) <+> text "?"

    inform $ text "Sind alle Ziffern im erlaubten Bereich?"
    B.range ( basis c ) z 
    inform $ text "Ja."
           
    when True -- ( normalisiert c )
	 $ assert ( F.ist_normalisiert z ) 
	 $ text "Ist die Zahl normalisiert?"
    assert ( size ( F.mantisse z ) <= max_stellen_mantisse c )
	   $ text "Hat die Mantisse hÃ¶chstens" 
                 <+> toDoc ( max_stellen_mantisse c )
	         <+> text "Stellen?"
    assert ( size ( F.exponent z ) <= max_stellen_exponent c )
	   $ text "Hat der Exponent hÃ¶chstens" 
                 <+> toDoc ( max_stellen_exponent c )
	         <+> text "Stellen?"

-- Local Variables: 
-- mode:haskell
-- End: 
