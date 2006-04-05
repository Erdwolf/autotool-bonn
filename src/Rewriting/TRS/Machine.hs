{-# OPTIONS -fglasgow-exts #-}

module Rewriting.Machine where

--   $Id$

import Machine.Class
import Machine.History

import Rewriting.TRS
import Rewriting.Steps

import Autolib.Set
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Size
import Data.Array
import Data.Maybe
import Data.List ( tails )

import Data.Typeable


data Derive v c = Derive [ Term v c ] deriving Typeable

instance ( Eq v, Eq c ) => Eq ( Derive v c ) where
    Derive (x : xs)  == Derive (y : ys) = x == y
instance ( Ord v, Ord c ) => Ord ( Derive v c ) where
    Derive (x : xs)  `compare` Derive (y : ys) = x `compare` y
instance ( ToDoc v, Symbol  c ) => ToDoc ( Derive v c ) where
    toDoc ( Derive (x :xs) ) = toDoc $ x

instance History ( Derive v c ) where
    history ( Derive (x : xs) ) = do
        ys <- tails xs
        guard $ not $ null ys
        return $ Derive xs

instance Symbol v 
        => Compute ( TRS v Identifier ) ( Derive v Identifier ) where
    next trs ( Derive xs) 
        = mkSet $ do 
            y <- successors trs $ head xs
            return $ Derive $ y : xs
    accepting trs ( Derive ( x : xs)) 
        = isJust $ result $ udecode x
    depth trs ( Derive (x : xs) ) = length xs

instance Symbol v
        => In  ( TRS v Identifier ) ( Term v Identifier ) ( Derive v Identifier ) where
    input_reporter trs t = return $ Derive [t]
instance Symbol v
     => Out ( TRS v Identifier ) ( Term v Identifier ) ( Derive v Identifier ) where
    output_reporter trs (Derive ( x:xs)) = return $ x

instance ToDoc v => Encode ( Term v Identifier )  where
    -- put argument list as term  f(x1, x2, ...)
    encode xs = Node ( mk ( length xs ) "f" )
              $ map uencode xs

instance ToDoc v => Decode ( Term v Identifier ) where
    -- get result from term over { S, Z }
    decode t = case result $ udecode t of
           Just i -> i
           Nothing -> error "Rewriting.Machine.decode"

s :: Identifier
s = mkunary "S"

z :: Identifier
z = mkunary "Z"

uencode :: Integer -> Term v Identifier
uencode k = foldr (.) id ( replicate ( fromIntegral k ) $ \ t -> Node s [t] ) 
        $ Node z [] 

udecode :: ToDoc v
        => Term v Identifier -> Reporter Integer
udecode ( Node f [ ] ) | f == z = return 0
udecode ( Node f [t] ) | f == s = do
    i <- udecode t ; return $ succ i
udecode t = reject 
          $ text "term" <+> toDoc t 
                <+> text "ist keine Unärdarstellung einer Zahl."
