{-# LANGUAGE MultiParamTypeClasses #-}
module Number.Wert where

class Num b => Wert a b where 
    wert :: a -> b

class Num b => Wert_at a b where 
    wert_at :: Int -> a -> b


