--  $Id$


module Partition.Central 

( Partition
, make_fixed
, make_quiz
)

where

import Challenger.Partial
import Inter.Types
import Inter.Quiz

import Partition.Beispiel
import Partition.Roll
import Partition.Param

import Autolib.ToDoc
import Autolib.Set
import Autolib.FiniteMap
import Autolib.Informed
import Autolib.Reporter
import Autolib.Reporter.Set

import Data.Typeable

data Partition = Partition
    deriving ( Eq, Typeable, Show, Read )

instance Partial Partition Conf ( Set Integer, Set Integer ) where

    describe Partition (Conf s) = vcat
        [ text "Zerlegen Sie die Menge"
	, nest 4 $ toDoc s
        , text "in zwei Teilmengen mit übereinstimmender Element-Summe."
	]

    initial Partition (Conf s) = 
        let dist :: [a] -> ( [a], [a] )
            dist [] = ( [], [] )
	    dist (x : xs) = let (here, there) = dist xs 
                            in  (x : there, here)
            ( this, that ) = dist $ setToList s
	in  ( mkSet this
	    , mkSet that 
	    )

    partial Partition (Conf s) (l, r) = do
        disjoint ("L", l) ("R", r)
        eq ( text "S" , s )
	   ( text "L + R" , union l r )

    total Partition (Conf s) (l, r) = do
        let sl = sum $ setToList l
	let sr = sum $ setToList r
        inform $ vcat
	       [ text "sum" <+> parens ( toDoc l ) 
	       , nest 4 $ text "=" <+> toDoc sl
	       , text "sum" <+> parens ( toDoc r ) 
	       , nest 4 $ text "=" <+> toDoc sr
	       ]
	assert ( sl == sr )
	       $ text "Stimmen die Summen überein?"

disjoint ( ix, x ) ( iy, y ) = do
    inform $ vcat
	   [ text "sind die Mengen"
	   , nest 4 $ text ix <+> equals <+> toDoc x
	   , text "und"
	   , nest 4 $ text iy <+> equals <+> toDoc y
	   , text "disjunkt?"
	   ]
    let xy = intersect x y
    if isEmptySet xy
       then inform $ text "ja."
       else reject $ text "nein, der Durchschnitt ist"
		   $$ toDoc xy


make_fixed :: Make
make_fixed = direct Partition Partition.Beispiel.mm

instance Generator Partition Param Conf where

    generator Partition p key = do
        let ( lo, hi ) = Partition.Param.bounds p
        xs <- Partition.Roll.rand
                    ( Partition.Param.elements p ) 
                    ( fromIntegral lo )
                    ( fromIntegral hi )
{- rand always returns a list from distinct elements
        let mkuni xs = mkSet $ do
             let fm = addListToFM_C (+) emptyFM $ do
                          x <- xs ; return ( x, 1 )
             ( x, count ) <- fmToList fm
             guard $ odd count
             return $ fromIntegral x
        return $ mkuni xs
-}
        return $ Conf $ mkSet $ map fromIntegral xs

instance Project Partition Conf Conf where project Partition = id

make_quiz :: Make
make_quiz = quiz Partition $ Partition.Param.p 
