module Code.Move_To_Front where

--  $Id$

import qualified Code.Type as T

import Code.Move_To_Front.Work
import Code.Move_To_Front.Data

import Autolib.ToDoc

coder :: Ord a
      => T.Coder a ( Coding [a] )
coder = T.Coder
      { T.nametag = text "MTF"
      , T.encode  = encode
      , T.decode = \ c -> Just $ decode c
      , T.decode_hint = \ c -> take 2 $ queue c
      }


