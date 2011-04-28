{-# LANGUAGE UndecidableInstances, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Code.Quiz where

import Code.Type
import Code.Class hiding ( enc, dec )
import Code.Param

import Inter.Types
import Inter.Quiz

import Autolib.Util.Wort
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Util.Size
import Autolib.Util.Seed

import System.Random
import Data.Typeable

-- testing
import qualified Code.Move_To_Front as MTF
import qualified Code.Burrows_Wheeler as BW

throw :: ( Ord a, Typeable a, Reader [a], ToDoc [a] )
      => Config a -> IO [a]
throw conf = do
    l <- randomRIO $ length_range conf
    w <- someIO ( setToList $ alphabet conf ) l
    return w

instance ( Reader a , Read a, Reader [a], Reader b, ToDoc c, Coder c a b ) 
     => Generator (Encode c) (Config a) [a] where
    generator _ conf key = do
        input <- throw conf
	return input

instance Project (Encode c) [a] [a] where
    project _ = id

enc :: ( ToDoc c, Reader c, Reader b, Coder c Char b, Read b ) => c -> Make
enc c = quiz (Encode c) Code.Param.example




instance ( Read a, Reader a, Reader [a], ToDoc a
	 , Read b, ToDoc c
	 , Coder c a b 
	 )
     => Generator (Decode c) (Config a) b where
    generator (Decode c) conf key = do

        -- advance Random generator (w.r.t. Encode)
        sequence_ $ replicate 10 $ randomRIO (False, True)

        input <- throw conf
	let output = encode c input
	return output

instance Project (Decode c) b b where
    project _ = id

dec :: ( ToDoc c, Reader c, Read b, Coder c Char b ) 
      => c -> Make
dec c = quiz (Decode c) Code.Param.example


