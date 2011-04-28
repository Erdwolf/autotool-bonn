{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FunctionalDependencies, MultiParamTypeClasses, FlexibleContexts #-} 

module Code.Type 

( module Code.Type
, module Autolib.FiniteMap
, module Autolib.Set
)

where


import Code.Param

import Data.Typeable

import Autolib.FiniteMap
import Autolib.Set
import Autolib.Size
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter

import Data.Maybe
import Data.Typeable

-- | FIXME: das ist unzusammenhängend

class  ( Typeable a, Typeable b, Typeable c
       , ToDoc [a], ToDoc b, Reader b
       , Reader [a]
       , Read a
       , Ord a , Eq b, BitSize b 
       ) 
       => Coder c a b | c a -> b, c b -> a where

     encode_hint :: c -> [a] -> b
     encode_hint c xs = encode c (take 3 xs)

     -- implement one of [ encode, encodeR ]
     encode      :: c -> [a] -> b
     encode c xs = fromMaybe (error "encode") $ result $ encodeR c xs

     encodeR      :: c -> [a] -> Reporter b
     encodeR c xs = do
         inform $ vcat [ text "Codierung von"
                       , nest 4 $ toDoc xs
                       ]
         let ys = encode c xs 
         inform $ vcat [ text "liefert"
                               , nest 4 $ toDoc ys
                               ]
         return ys

     decode_hint :: c -> b ->       [a]
     
     -- implement one of [ decode, decodeR ]
     decode      :: c -> b -> Maybe [a]
     decode c b = result $ decodeR c b

     decodeR     :: c -> b -> Reporter [a]
     decodeR c b = do
         inform $ vcat [ text "Decodierung von"
                       , nest 4 $ toDoc b
                       ]
         case decode c b of
             Just xs -> do
                 inform $ vcat [ text "liefert"
                               , nest 4 $ toDoc xs
                               ]
                 return xs
             Nothing -> do
                 reject $ text "ist nicht möglich."

class Size b => BitSize b where
      bitSize :: b -> Integer
      bitSize = fromIntegral . size

bits :: ( Integral a, Integral b ) 
     => a -> b
bits x | x > 1 = 1 + bits (x `div` 2 )
bits _ = 1


data  Ord a => Frequency a = Frequency ( FiniteMap a Int )
    deriving Typeable

instance Ord a => Size ( Frequency a ) where 
    size ( Frequency fm ) = size fm

instance ( Ord a, ToDoc a ) => ToDoc ( Frequency a ) where
    toDoc ( Frequency fm ) = text "Frequency" </> toDoc ( fmToList fm )
instance ( Ord a, Reader a ) => Reader ( Frequency a ) where
    reader = do
        my_reserved "Frequency"
        fm <- reader
        return $ Frequency $ listToFM fm

data Ord a => Code a b =  Code ( FiniteMap a [ b ] )
    deriving Typeable

instance Ord a => Size ( Code a b ) where 
    size ( Code fm ) = size fm

instance ( Ord a, ToDoc a, ToDoc b ) => ToDoc ( Code a b ) where
    toDoc ( Code fm ) = text "Code" </> toDoc ( fmToList fm )
instance ( Ord a, Reader a, Reader b ) => Reader ( Code a  b) where
    reader = do
        my_reserved "Code"
        fm <- reader
        return $ Code $ listToFM fm


-- local variables:
-- mode: haskell
-- end;



