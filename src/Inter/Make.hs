module Inter.Make where

--  $Id$

import Inter.Types
import Data.Typeable
import Data.Dynamic
import Data.Maybe


import Autolib.ToDoc
import Autolib.Reader
import Text.XML.HaXml.Haskell2Xml


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

-- | für den tutor: präsentiert liste der bekannten maker-typen
present :: [ Make ] -> Doc
present makers = vcat $ do
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



		   
    
    