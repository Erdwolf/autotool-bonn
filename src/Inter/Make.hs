module Inter.Make where

--  $Id$

import Inter.Types
import Data.Typeable
import Data.Dynamic
import Data.Maybe

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Informed
import Text.XML.HaXml.Haskell2Xml

data Make = forall conf p i b 
          . ( V p i b 
	    , Typeable conf, Haskell2Xml conf, ToDoc conf, Show conf, Reader conf
	    )
	  => Make String -- ^ description
		  (conf -> Var p i b) -- ^ maker function
                  conf -- ^ example

get_boiler :: [ Make ] 
	   -> [ Dynamic ]
	   -> [ Variant ]
get_boiler makers configs = do
    dyn <- configs
    maker <- makers
    case maker of
        Make doc ( fun :: conf -> Var p i b ) ex -> do
	    it <- maybeToList ( fromDynamic dyn :: Maybe conf )
	    return $ Variant $ fun it

-- | f��r den tutor: pr��sentiert liste der bekannten maker-typen
present :: [ Make ] -> Doc
present makers = hcat $ do
    maker <- makers
    case maker of
        Make doc ( fun :: fun ) ex -> return $ text $ show 
	       $ hsep [ text doc, comma, toDoc $ typeOf fun ]

cpresent :: [ Make ] -> [ Dynamic ] -> Doc
cpresent makers configs = hcat $ do
    dyn <- configs
    maker <- makers
    case maker of
        Make doc ( fun :: conf -> Var p i b ) ex -> do
	    it <- maybeToList ( fromDynamic dyn :: Maybe conf )
	    return $ text $ showXml it



		   
    
    