module Baum.Type 

( module Baum.Type
, module TES
, module TES.Identifier 
, module Util.Size
)

where

--  $Id$

import TES hiding ( Var )
import TES.Identifier ( Identifier, mkunary )
import qualified Data.Tree as D

import Reporter
import ToDoc
import Util.Size

type Baum = Term () Identifier

mkTree :: ( Show c, ToDoc c ) 
       => Term v c -> D.Tree c
mkTree ( Node f args ) = D.Node f $ map mkTree $ reverse args

present :: ( Show c, ToDoc c, ToDoc v )
	=> Term v c -> Doc
present t = 
    vcat [ toDoc t
	 , text ""
	 , nest 4 . vcat . map text . lines . D.drawTree . mkTree $ t
	 ]
