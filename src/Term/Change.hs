module Term.Change where

--   $Id$

import Term.Type
import Term.Match



import Maybe
import Schichten
import Data.Set
import Data.FiniteMap
import Control.Monad

age :: (Int, a) -> Int
age = fst

base :: (Int, a) -> a
base = snd

aged :: Int -> (Term a) -> Term (Int, a)
aged a = fmap ( \ x -> (a,x) )

young :: (Term a) -> Term (Int, a)
young = aged 0


link :: Eq a 
     => Term (Int, a) -> Term (Int, a) -> Term (Int, a)
-- bei nichtlinearen regeln:
-- wie werden zwei verschiedene belegungen einer variablen kombiniert?
-- das maximum der alter nehmen?
link l r = 
    if base (symbol l) == base (symbol r)
    then Term { symbol = ( min (age $ symbol l) (age $ symbol r)
			 , base (symbol l) )
	      , children = zipWith link ( children l) (children r)
              }
    else error "Term.Change.link"

next :: Eq a 
     => TRS a -> Term (Int, a) -> [ Term (Int, a) ]
next trs t = do
    p <- positions t
    let s = peek t p

    -- erstmal matchen und dabei alter ignorieren
    let s0 = fmap snd s
    (l, r) <- trs
    sub <- maybeToList $ match l s0

    -- jetzt die alters-information für die match-belegungen aufsammeln
    let sub' = addListToFM_C link emptyFM $ do
	    q <- positions l
	    let v = peek l q
	    guard $ isvar v
	    return ( case symbol v of Left n -> n , peek s q )
	    
    -- alters-information für das match-gerüst
    let m = minimum $ do
	    q <- positions l
	    guard $ not $ isvar $ peek l q
	    -- tatsächliches alter einer nonvar-pos
	    return $ age $ symbol $ peek s q 
    let m' = 1 + m
    -- gealtertes gerüst bauen
    let r' = fmap ( \ f -> case f of 
		    Left v -> Left v -- variable bleibt
		    Right ff -> Right (m', ff) -- symbol mit alter
		  ) r

    return $ poke t p $ apply sub' r'

successors :: Ord a => TRS a -> Term (Int, a) -> [ Term (Int, a) ]
-- 0, 1, .. schritte (evtl unendliche liste!)
successors trs t = do
    ts <- schichten ( mkSet . next trs ) t
    setToList ts

leftmost :: Ord a => TRS a -> Term (Int, a) -> [ Term (Int, a) ]
leftmost srs t = t : 
    case next srs t of
         [] -> []
	 (s : _) -> leftmost srs s
