module Inter.Validate where

-- $Id$

import Inter.Types
import qualified Inter.Param as P
import Text.Html
import SQLqueries

import Monad ( guard )

validate :: P.Type -> IO ( Either Html P.Type )
validate par = do
    mbsnr <- loginDB (P.matrikel par) (show $ P.passwort par)
    case mbsnr of 
         Nothing -> return $ Left $ p << "Passwort paßt nicht"
	 Just snr -> do 
	      let vvs :: [ Variant ]
		  vvs = do
		      vv <- P.variants par
		      case vv of 
			  Variant v -> do
 			      guard $ P.variant par  
				      ==        Inter.Types.aufgabe v
 			      return vv
	      case vvs of
		   [ vv ] -> return $ Right $ par { P.snr = snr
						  , P.variante = vv
						  }
		   _      -> return $ Left 
		                    $ p << "mögliche Aufgaben:"
				    +++ p << show ( P.variants par )




