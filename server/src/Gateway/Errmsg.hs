module Gateway.Errmsg where

--   $Id$

import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos
import Autolib.ToDoc

errmsg :: Int -> ParseError -> String -> Doc
errmsg input_width e inp = 
    let css = lines inp
 	p = errorPos e
 	(pre, post) = splitAt (sourceLine p ) css
 	w = input_width ; c = sourceColumn p - 1
       	top = replicate c '.' ++ replicate (w-c) '^' ; bot = replicate w '-' 
    in  vcat $ map Autolib.ToDoc.text 
 	     $ pre ++ [ top ]
 		   ++ [ showErrorMessages 
 			      "oder" -- "or" 
 			      "unbekannter Parser-Fehler" -- "unknown parse error" 
                               "möglich ist hier:" -- "expecting" 
 			      "falsches Zeichen:" -- "unexpected" 
 			      "Ende der Eingabe" -- "end of input"
                        (errorMessages e) ]
 		   ++ [ bot ]
 		   ++ post
