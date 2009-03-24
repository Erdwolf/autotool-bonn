{-# language UndecidableInstances, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module Graph.Van_der_Waerden where

import Graph.Util

import Autolib.Graph.Ops ( complement )
import Autolib.Graph.Basic

import Inter.Types
import Autolib.ToDoc
import Autolib.Size
import Autolib.FiniteMap
import Autolib.Set
import qualified Challenger as C

import Data.Typeable
import Data.List
import Data.Maybe

data Van_der_Waerden = Van_der_Waerden deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Verify Van_der_Waerden (Int,Int) where
    verify p (r,k) = do
        when ( r <= 0 ) 
             $ reject 
             $ text "der erste Parameter (r = Farbzahl) soll > 0 sein"
        when ( k <= 0 ) 
             $ reject 
             $ text "der zweite Parameter (k = Folgenlänge) soll > 0 sein"

-- | all arithmetic sequences of length k
-- with elements in [ 0 .. n - 1 ]
arithmetic_sequences :: Int -> Int -> [[Int]]
arithmetic_sequences n k | k > 0 = do
    start <- [ 0 .. n - 1 ]
    diff  <- [ 1 .. n ]
    let end = start + (k-1) * diff
    guard $ end < n
    return [ start, start + diff .. end ]

instance C.Partial Van_der_Waerden (Int,Int) [Int] where

    describe p (r,k) = vcat $
        [ text "Gesucht ist eine möglichste lange Folge"
        , text "über dem Alphabet" <+> toDoc [ 1 .. r ]
	, text "ohne einfarbige arithmetische Teilfolgen der Länge" <+> toDoc k
	] 

    initial p (r,k) = take (1 + r*k) 
        $ map ( \ x -> 1 + x `mod` r )
        $ map fromEnum
        $ show $ C.describe p (r,k)

    partial p (r,k) xs = do
        let wrong = do
             (i,x) <- zip [ 0 :: Int .. ] xs
             guard $ x < 1 || x > r
             return (i,x)
        when ( not $ null wrong ) $ reject $ vcat
             [ text "auf diesen Positionen steht eine nicht erlaubte Farbe:"
             , nest 4 $ toDoc wrong
             ]
    
    total p (r,k) xs = sequence_ $ do 
        ys <- arithmetic_sequences (length xs) k
        return $ do
            let cs = Data.List.nub $ do y <- ys ; return $ xs !! y
            when ( length cs <= 1 ) $ reject $ vcat
                [ text "die arithmetische Indexfolge ist einfarbig:"
                , nest 4 $ toDoc ys
                ]

instance C.Measure Van_der_Waerden (Int,Int) [Int] where 
    measure p (r,k) xs = fromIntegral $ length xs

make :: Make
make = direct Van_der_Waerden (3::Int,4::Int)
