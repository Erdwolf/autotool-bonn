module Baum.Binary.Show where

--  $Id$

import Baum.Binary.Type
import Data.Tree

toTree :: Show a => Baum a -> Tree String
toTree Null = Node "null" []
toTree b = Node ( show $ key b )
	        [ toTree ( right b ) , toTree ( left b ) ]



        