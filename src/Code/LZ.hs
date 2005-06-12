module Code.LZ 

( Lempel_Ziv_Welch (..) )

where

import qualified Code.Type as T

import Code.LZ.Data
import Code.LZ.Book

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
    encode _ [] = []
    encode _ (x : xs) = output 
                $ ( flip execState ) blank 
                $ enc ( [x], Letter x ) xs
    encode_hint lzw xs = take 3 $ encode lzw xs
    decodeR _ xs       = dec ( leer, [] ) xs
    decode_hint lzw ys = take 3 
                       $ fromJust (error "decode_hint") $ decode lzw ys


-- | insert word into book
-- precondition: word is not in book
extend :: ( ToDoc a, ToDoc [a], Reader a , Reader [a]
          , ToDoc b, ToDoc [b], Reader b , Reader [b]
          , Ord a 
          )
       => [a] -> State ( Cache a b ) ( )
extend w = do
    c <- get
    put $ c { book = insert ( book c ) w  }

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
    ) => ( [a], Code_Letter a )
    -> [a]
    -> State ( Cache a (Code_Letter a) ) ( )
enc (w, prev) [] = do
    out prev
enc (w, prev) (x : rest) = do
    let wx = w ++ [x]
    c <- get
    case Code.LZ.Book.lookup ( book c ) wx of
         Nothing -> do
             extend wx
             out prev
             enc ( [x], Letter x ) rest
         Just i  -> do
             enc ( wx, i ) rest

--------------------------------------------------------------------

header fm c = do
    inform $ vcat
	   [ text "aktuelles Buch:" <+> toDoc fm
	   , text "bearbeite:" <+> toDoc c
	   ]

dec :: ( Ord a, ToDoc a, ToDoc [a], Reader [a] )
    => ( Book a , [a] )
    -> [ Code_Letter a ]
    -> Reporter [ a ]
dec ( b, s ) [] = do
    return []
dec ( b, s ) (c @ (Letter x) : xs) = do
    header b c
    let b' = insert b ( s ++ [x] )
    rest <- dec ( b', [x] ) xs
    return $ x : rest
dec (b, s) (c @ (Entry i) : xs) = do
    header b c
    ( w, bs' ) <- case lookupFM ( inverse $ long b ) i of
        Nothing | i == sizeFM ( long b ) -> do
            inform $ text "neuer Eintrag"
            let w' = s ++ take 1 s
            return ( s , ( insert b w', s ) )
	Nothing -> reject $ vcat
                          [ text "Es gibt keinen Eintrag" <+> toDoc i 
                          , text "im aktuellen Wörterbuch" <+> toDoc b
                          ]
        Just w -> do
            -- known entry
            inform $ text "entspricht Zeichenkette" <+> toDoc w
            let w' = s ++ take 1 w
            return ( w, ( insert b w', w ) )
    rest <- dec bs' xs
    return $ w ++ rest

------------------------------------------------------------

inverse :: ( Ord a, Ord b ) => FiniteMap a b -> FiniteMap b a
inverse fm = listToFM $ do 
   (x, y) <- fmToList fm
   return ( y, x )

