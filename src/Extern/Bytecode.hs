module Extern.Bytecode where

-- -- $Id$

import qualified Posix
import Util.Datei

bytecode :: Int -> String -> IO String
bytecode argc input = do
    let args = show $ parens $ punctuate comma
	            $ do i <- [ 1 .. argc ]
			 return $ text "int" <+> text "x" <> toDoc i
    let code = [ "class Check {"
	       , "    public static check " ++ args ++ "{"
	       , input
	       , "    }"
	       , "}"
	       ]
    pid <- Posix.getProcessId
    let file ext = Datei { pfad = [ "autotool", "tmp", pid ]
			  , name = "Check." ++ ext
			  , relativzahl = error "Extern.Javap.relativzahl"
			  }
    schreiben ( file "java" ) code
    jpath <- home $ file "java"
    cpath <- home $ file "class"
    opath <- home $ file "out"
    system $ unwords [ "/usr/bin/javac" , jpath ]
    system $ unwords [ "/usr/bin/javac" , "-c", cpath , ">" , opath ]
    output <- lesen ( file "out" )

    -- TODO: files/directory löschen

    -- nach methode suchen

    return output
