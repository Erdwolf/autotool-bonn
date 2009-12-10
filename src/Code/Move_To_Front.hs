module Code.Move_To_Front ( Move_To_Front (..) ) where

--  $Id$

import qualified Code.Type as T

import qualified Code.Move_To_Front.Work as W
import Code.Move_To_Front.Data

import Code.Type
import Code.Param

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable

instance ( Typeable a, Ord a, ToDoc [a], Reader [a], Read a ) 
        => Coder Move_To_Front a ( Coding [a] ) where
      encode c = W.encode
      decode c it = Just $ W.decode it
      decode_hint c it = take 2 $ queue it
      


