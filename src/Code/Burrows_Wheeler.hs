module Code.Burrows_Wheeler ( Burrows_Wheeler (..) ) where

--  $Id$

import qualified Code.Type as T
import Code.Burrows_Wheeler.Work
import Code.Burrows_Wheeler.Data

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size
import Data.Typeable

{-
coder :: Ord a
      => T.Coder a ( [a], Int )
coder = T.Coder
      { T.nametag = text "BW"
      , T.encode = bw
      , T.decode = error "Burrows-Wheeler.decode"
      , T.decode_hint = \ ( w, i ) -> reverse $ take 3 w
      }
-}

instance ( Typeable a, Ord a, ToDoc [a] , Reader [a], Size a) 
        => T.Coder Burrows_Wheeler a ( [a], Int ) where
      encode c = bw
      decode c = error "Burrows-Wheeler.decode"
      decode_hint c = \ ( w, i ) -> reverse $ take 3 w

