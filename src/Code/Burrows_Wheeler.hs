module Code.Burrows_Wheeler where

-- $Id$

import qualified Code.Type as T
import Code.Burrows_Wheeler.Work

import ToDoc

coder :: Ord a
      => T.Coder a ( [a], Int )
coder = T.Coder
      { T.nametag = text "Burrows-Wheeler"
      , T.encode = bw
      , T.decode = error "Burrows-Wheeler.decode"
      , T.decode_hint = \ ( w, i ) -> reverse $ take 3 w
      }
