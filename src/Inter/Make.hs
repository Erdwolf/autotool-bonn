module Inter.Make where

--  $Id$

import Inter.Types
import Data.Typeable
import Data.Dynamic
import Data.Maybe

import ToDoc

data Make = forall conf p i b 
          . ( V p i b , Typeable conf )
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

present :: [ Make ] -> Doc
present makers = vcat $ do
    maker <- makers
    case maker of
        Make ( fun :: fun ) -> text $ show $ typeOf fun

		   
    
    