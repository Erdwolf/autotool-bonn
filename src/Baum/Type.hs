#if (__GLASGOW_HASKELL__ >= 604)
{-# OPTIONS -fallow-incoherent-instances #-}
#endif

module Baum.Type 

( module Baum.Type
, module Autolib.TES
, module Autolib.TES.Identifier 
, module Autolib.Size
)

where

--  $Id$

import Autolib.TES hiding ( Var )
import qualified Autolib.TES.Draw
import Autolib.TES.Identifier ( Identifier, mkunary )
import qualified Data.Tree as D

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Size

type Baum = Term () Identifier

{-
mkTree :: ( Show c, ToDoc c ) 
       => Term v c -> D.Tree c
mkTree ( Node f args ) = D.Node f $ map mkTree $ reverse args
-}

present :: ( Symbol c, ToDoc v )
	=> Term v c -> Doc
present t = 
    vcat [ toDoc t
	 , text ""
	 , nest 4 $ Autolib.TES.Draw.draw t
	 ]
