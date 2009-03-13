module Program.General.Value where

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable

class ( ToDoc val, Eq val, Typeable val ) => Value val where 
    typeform :: val -> Doc
    typeread :: Parser ( Parser val )
