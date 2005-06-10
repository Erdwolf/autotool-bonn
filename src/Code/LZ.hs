module Code.LZ 

( Lempel_Ziv_Welch (..) )

where

import qualified Code.Type as T

import Code.LZ.Data

import Code.Type
import Code.Param

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter hiding ( output )
import Autolib.FiniteMap

import Data.Typeable
import Data.Maybe
import Control.Monad.State

instance ( Typeable a, Ord a, ToDoc a, Reader a, ToDoc [a], Reader [a] )
    => Coder Lempel_Ziv_Welch a [ Code_Letter a ] where
    encode _ xs = output 
                $ ( flip execState ) blank 
                $ enc ( take 1 xs, undefined ) ( drop 1 xs )
    encode_hint lzw xs = take 3 $ encode lzw xs
    decodeR _ xs       = dec emptyFM xs
    decode_hint lzw ys = take 3 
                       $ fromJust (error "decode_hint") $ decode lzw ys


-- | implicitely, all one-letter words are in the book (without numbers): 
-- if w has length one, return Letter w
-- else look in Book
look :: Ord a => FiniteMap [a] Int -> [a] -> Maybe ( Code_Letter a )
look fm [x] = Just $ Letter x
look fm w   = fmap Entry $ lookupFM fm w

-- | insert word into book
-- precondition: word is not in book
extend :: ( ToDoc a, ToDoc [a], Reader a , Reader [a]
          , ToDoc b, ToDoc [b], Reader b , Reader [b]
          , Ord a 
          )
       => [a] -> State ( Cache a b ) ( )
extend w = do
    c <- get
    let i = sizeFM $ book c
    put $ c { book = addToFM ( book c ) w i }

-- | append letter to output
out :: ( ToDoc a, ToDoc [a], Reader a , Reader [a]
          , ToDoc b, ToDoc [b], Reader b , Reader [b]
          , Ord a 
          )
     => b -> State ( Cache a b ) ()
out cl = do
    c <- get 
    put $ c { output = output c ++ [ cl ] }

---------------------------------------------------------------------------

enc :: ( ToDoc a, ToDoc [a], Reader a , Reader [a], Ord a
    ) => ([a], Code_Letter a) 
    -> [a] 
    -> State ( Cache a (Code_Letter a) ) ( )
enc ([], _) [] = do
    return ()
enc (w, prev) [] = do
    out prev
enc (w, prev) (x : rest) = do
    let wx = w ++ [x]
    c <- get
    case look ( book c ) wx of
         Nothing -> do
             extend wx
             out prev
             enc ( [x], Letter x ) rest
         Just i  -> do
             enc ( wx, i ) rest

--------------------------------------------------------------------

dec :: ( Ord a, ToDoc a, ToDoc [a] )
    => FiniteMap Int [a] 
    -> [ Code_Letter a ]
    -> Reporter [ a ]
dec fm [] = do
    return []
dec fm (c @ (Letter x) : xs) = do
    inform $ text "bearbeite:" <+> toDoc c
    rest <- dec fm xs
    return $ x : rest
dec fm (c @ (Entry i) : xs) = do
    inform $ text "bearbeite:" <+> toDoc c
    w <- case lookupFM fm i of
        Nothing -> reject $ vcat
                          [ text "Es gibt keinen Eintrag" <+> toDoc i 
                          , text "im aktuellen Wörterbuch" <+> toDoc fm
                          ]
        Just w -> do
            inform $ text "entspricht Zeichenkette" <+> toDoc w
            return w
    rest <- dec ( addToFM fm ( sizeFM fm ) w ) xs
    return $ w ++ rest


