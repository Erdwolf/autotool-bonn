module Inter.Validate where

-- $Id$

import Inter.Types
import qualified Inter.Param as P

-- das wird später mal autotool/bin/Settings.hs
import qualified Inter.Param as S 

import Text.Html
import SQLqueries

import Monad ( guard )
import ToDoc

validate :: P.Type -> IO ( Either Html P.Type )
validate par = do
    mbsnr <- loginDB (P.matrikel par) (show $ P.passwort par)
    case mbsnr of 
         Nothing -> return $ Left $ p << "Passwort paßt nicht"
	 Just ident -> do 
	      let vvs :: [ Variant ]
		  vvs = do
		      vv <- P.variants par
		      case vv of 
			  Variant v -> do
 			      guard $ P.problem par  
				      == show ( Inter.Types.problem v )
 			      guard $ P.aufgabe par  
				      ==        Inter.Types.aufgabe v
 			      guard $ P.version par  
				      ==        Inter.Types.version v
 			      return vv
	      case vvs of
		   [ vv ] -> continue $ par { P.ident = ident
						  , P.variante = vv
						  }
		   _      -> return $ Left 
		                    $ p << "mögliche Probleme/Aufgaben/Versionen:"
				    +++ pre << render ( toDoc $ P.variants par )

continue :: P.Type -> IO ( Either Html P.Type )
continue set = do
   let is_admin = read ( S.matrikel set ) < 1000
   aufs <- mglAufgabenDB' is_admin $ S.ident set
   let matching = do
         auf @ ( anr, name, subject, path, highscore ) <- aufs
	 guard $ name == S.aufgabe set && subject == S.version set
	 return auf
   case matching of
	 [ auf @ ( anr, name, subject, path, highscore ) ] -> 
	     return $ Right 
		    $ set { S.anr = anr , S.highscore = read highscore }
	 _ -> return $ Left
		           $ p << "Diese Aufgabe/Version ist nicht aktuell."
			   +++ p << pre << render ( toDoc matching )






