module Baum.Such.Central where

--  $Id$

import Baum.Such.Config
import Baum.Such.Class
import Baum.Such.Op
import Baum.Such.Inter
import Baum.Such.Generate
import Baum.Binary

import Challenger.Partial
import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader

import Inter.Types
import Inter.Quiz
import Data.Typeable

data Suchbaum = Suchbaum deriving ( Eq, Ord, Show, Read, Typeable )

instance ( Such baum, OpC a ) => 
    Measure Suchbaum ( Instanz baum a ) [ Op a ] where
    measure Suchbaum inst ops = fromIntegral $ length ops

instance ( Such baum, OpC a, ToDoc (baum a) ) => 
    Partial Suchbaum ( Instanz baum a ) [ Op a ] where

    describe Suchbaum ( start, plan, end ) = vcat
       [ text "Auf den Baum:"
       , nest 4 $ form start
       , text "sollen diese Operationen angewendet werden"
       , text "(wobei Sie  Any  geeignet ersetzen sollen):"
       , nest 4 $ toDoc plan
       , text "so daﬂ dieser Baum entsteht:"
       , nest 4 $ form end
       ]

    initial Suchbaum ( start, plan, end ) =
        plan

    total   Suchbaum ( start, plan, end ) ops = do
        inform $ text "Beginne mit" <+> form start
        c <- steps start plan ops
	assert ( c `equal` end) $ vcat
	       [ text "Stimmt ¸berein mit Aufgabenstellung?"
	       , nest 4 $ form end
	       ]

instance Generator Suchbaum ( Config Int ) ( Instanz Baum Int ) where
    generator Suchbaum conf key = generate conf

instance Project Suchbaum  ( Instanz Baum Int ) ( Instanz Baum Int ) where
    project Suchbaum i = i

make_quiz :: Make
make_quiz = quiz Suchbaum Baum.Such.Config.example


