module Inter.Make where

--  $Id$

import Inter.Types
import Data.Typeable
import Data.Dynamic
import Data.Maybe

import ToDoc
import Text.XML.HaXml.Haskell2Xml

data Make = forall conf p i b 
          . ( V p i b 
	    , Typeable conf, Haskell2Xml conf
	    )
	  => Make (conf -> Var p i b)

get_boiler :: [ Make ] 
	   -> [ Dynamic ]
	   -> [ Variant ]
get_boiler makers configs = do
    dyn <- configs
    maker <- makers
    case maker of
        Make ( fun :: conf -> Var p i b ) -> do
	    it <- maybeToList ( fromDynamic dyn :: Maybe conf )
	    return $ Variant $ fun it

-- | für den tutor: präsentiert liste der bekannten maker-typen
present :: [ Make ] -> Doc
present makers = hcat $ do
    maker <- makers
    case maker of
        Make ( fun :: fun ) -> return $ text $ show $ toDoc $ typeOf fun

cpresent :: [ Make ] -> [ Dynamic ] -> Doc
cpresent makers configs = hcat $ do
    dyn <- configs
    maker <- makers
    case maker of
        Make ( fun :: conf -> Var p i b ) -> do
	    it <- maybeToList ( fromDynamic dyn :: Maybe conf )
	    return $ text $ showXml it



		   
    
    