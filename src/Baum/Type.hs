module Baum.Type 

( module Baum.Type
, module TES
, module Util.Size
)

where

--  $Id$

import TES
import TES.Identifier
import qualified Data.Tree as D

import Reporter
import ToDoc
import Util.Size

type Baum = Term () Identifier

mkTree :: Baum -> D.Tree Identifier
mkTree ( Node f args ) = D.Node f $ map mkTree $ reverse args

present :: Baum -> Doc
present t = 
    vcat [ toDoc t
	 , nest 4 . vcat . map text . lines . D.drawTree . mkTree $ t
	 ]
