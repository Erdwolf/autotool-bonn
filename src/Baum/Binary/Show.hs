module Baum.Binary.Show where

import Baum.Binary.Type
import Data.Tree

import Autolib.ToDoc

data Container a = Genau a | Kein

instance Show a => Show (Container a) where
    show (Genau a) = show a
    show (Kein   ) = "null"

toTree :: Show a => Baum a -> Tree (Container a)
toTree Null = Node Kein []
toTree b = Node ( Genau $ key b )
	        [ toTree ( right b ) , toTree ( left b ) ]

instance Show a => ToDoc (Tree a) where
    toDoc t = vcat $ map text $ lines $ show t

        