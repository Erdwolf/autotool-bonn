module Baum.Such.Central where

--  $Id$

import Baum.Such.Config
import Baum.Such.Class
import Baum.Such.Op
import Baum.Such.Inter
import Baum.Such.Generate

-- import qualified Baum.Binary as B
-- import qualified Baum.ZweiDrei as Z

import Challenger.Partial
import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader

import Inter.Types
import Inter.Quiz
import Data.Typeable


instance ( Tag t baum a ) => 
    Measure t ( Instanz baum a ) [ Op a ] where
    measure t inst ops = fromIntegral $ length ops

instance ( Tag t baum a ) => 
    Partial t ( Instanz baum a ) [ Op a ] where

    describe t ( start, plan, end ) = vcat
       [ text "Auf den Baum:"
       , nest 4 $ form start
       , text "sollen diese Operationen angewendet werden"
       , text "(wobei Sie  Any  geeignet ersetzen sollen):"
       , nest 4 $ toDoc plan
       , text "so daﬂ dieser Baum entsteht:"
       , nest 4 $ form end
       ]

    initial t ( start, plan, end ) =
        plan

    total   t ( start, plan, end ) ops = do
        inform $ text "Beginne mit" <+> form start
        c <- steps start plan ops
	assert ( c `equal` end) $ vcat
	       [ text "Stimmt ¸berein mit Aufgabenstellung?"
	       , nest 4 $ form end
	       ]

instance Tag t baum a
      => Generator t ( Config a ) ( Instanz baum a ) where
    generator t conf key = generate conf

instance Project t  ( Instanz baum a ) ( Instanz baum a ) where
    project t i = i

make_quiz :: Tag t baum Int => t -> Make
make_quiz t = quiz t Baum.Such.Config.example  




