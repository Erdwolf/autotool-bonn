#if (__GLASGOW_HASKELL__ >= 604)
{-# OPTIONS -fallow-incoherent-instances #-}
#endif

module NFA.Equiv.Core where

--  $Id$

import Autolib.NFA

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Util.Fix

import Autolib.Dot.Dotty ( peng )

import Autolib.Set
import Autolib.FiniteMap
import Data.List (partition, tails)
import Data.Maybe (fromMaybe,maybeToList)


type Klassen s = Set (Set s)
type Mappe   s = FiniteMap s Int

type Trenner c s = ( s, s, c )

----------------------------------------------------------------------


toMappe :: Ord s => Klassen s -> Mappe s
toMappe xss = listToFM $ do
    ( k, xs ) <- zip [1 .. ] $ setToList xss
    x <- setToList xs
    return ( x, k )

toKlassen :: Ord s => Mappe s -> Klassen s
toKlassen fm = mkSet $ eltsFM $ addListToFM_C union emptyFM $ do
    ( x, k ) <- fmToList fm
    return ( k, unitSet x )

----------------------------------------------------------------------

-- | verfeinert äquivalenzklassen
refine :: Ord s => (s -> s -> Bool) -> Klassen s -> Klassen s
refine eq xss = mkSet $ do xs <- setToList xss 
			   split eq $ setToList xs 

split :: Ord s => (s -> s -> Bool) -> [s] -> [Set s]
split eq [] = []
split eq (x : xs) =
    let (yeah, noh) = partition (eq x) xs
    in	mkSet (x : yeah) : split eq noh


-- | wer hier vorkommt, ist nicht äquivalent
mkTafel :: (Ord c, Ord s) => [ Trenner c s ] -> Set (s,s) 
mkTafel ts = mkSet $ do
    (p,q,c) <- ts
    [(p,q),(q,p)]

anwende :: ( Ord c, Ord s ) => Klassen s -> [ Trenner c s ] -> Klassen s
anwende xss ts = 
    let tafel = mkTafel ts
	eq x y = not (elementOf (x,y) tafel)
    in	refine eq xss

----------------------------------------------------------------------

compact :: NFAC c s
	=> NFA c s -> Klassen s -> NFA c (Set s)
compact a xss =
    let fm = listToFM $ do
	     xs <- setToList xss
	     x <- setToList xs
	     return ( x, xs )
        fun = fromMaybe (error "NFA.Equiv.compact") . lookupFM fm
    in  statemap fun a

----------------------------------------------------------------------

-- | das berechnet alle
trenner :: NFAC c s
        => Set c -> NFA c s -> Klassen s -> [ Trenner c s ]
trenner sigma a xss = do
    let fm = toMappe xss
    xs <- setToList xss
    x : ys <- tails $ setToList xs
    y <- ys
    c <- setToList sigma
    guard $ not $ eqqu c a fm x y
    return ( x, y, c )
    
eqqu ::  NFAC c s
     => c -> NFA c s -> Mappe s -> (s -> s -> Bool)
eqqu c a fm p q = 
    let x = nachfolger a p c
	y = nachfolger a q c
    in lookupFM fm x == lookupFM fm y

nachfolger :: NFAC c s
	   => NFA c s -> s -> c -> s
nachfolger a p c = 
    let qs = fromMaybe ( error "NFA.Equiv: nicht in trans" ) 
	   $  lookupFM (trans a) (p,c)
    in	case setToList qs of
	     [ q ] -> q
	     qs	 -> error "NFA.Equiv: nicht genau ein Nachfolger"

-----------------------------------------------------------------------------

check_trenner :: NFAC c s
	      => NFA c s -> Mappe s -> Trenner c s 
	      -> Reporter ()
check_trenner a fm (p,q,c) = do
    let p' = nachfolger a p c
    let q' = nachfolger a q c
    let st p c p' = fsep [ text "f"
			 , parens $ fsep $ punctuate comma
					 [ toDoc p, toDoc c ]
			 , equals, toDoc p' 
			 ]
    let flag = lookupFM fm p' == lookupFM fm q'
    let msg = fsep [ text $ if flag then "falsch :" else "richtig:"
		   , st p c p'
		   , text "und"
		   , st q c q'
		   , text "sind"
		   , if flag then empty else text "nicht"
		   , text "äquivalent"
		   ]
    if flag
       then do reject $ msg
       else do inform $ msg


check_trenners :: NFAC c s
	      => NFA c s -> Mappe s -> [ Trenner c s ]
	      -> Reporter ()
check_trenners a fm ts = mapM_ (check_trenner a fm) ts

----------------------------------------------------------------------

-- | prüfe einen schritt
schritt :: NFAC c s
	=> Set c -> NFA c s 
	-> Klassen s  -> (Int, [ Trenner c s ])
	-> Reporter ( Klassen s )
schritt sigma a xss (k, ts) = do
    let fm = toMappe xss
    inform $ text $ unwords [ "Schritt", show k ]
    nested 4 $ do
        inform $ text "Die Äquivalenzklassen sind:" <+> toDoc xss
        inform $ text "Ich prüfe Ihre trennenden Tupel:" <+> toDoc ts

        inform $ text "sind alle Tupel korrekt?"
        nested 4 $ check_trenners a fm ts

        inform $ text "sind alle für diesen Schritt nötigen Tupel vorhanden?"
        let ts' = trenner sigma a xss
        if isEmptySet (mkTafel ts' `minusSet` mkTafel ts)
            then do 
		 inform $ text "Ja." 
                 let cls = anwende xss ts 
                 let ca = compact a cls
		 inform $ vcat 
			[ text "Durch Zusammenfassen der bis hier äquivalenten Zustände"
			, text "ergibt sich dieser Automat"
			, text "(ist im Allgemeinen nicht deterministisch"
			, text "und nicht äquivalent zur Eingabe):"
			]
		 peng ca
	         return $ cls
            else do reject $ text "Nein. Sie müssen noch mehr Zustände trennen."

----------------------------------------------------------------------------

equiv :: NFAC c s
	=> String
	 -> Set c -> NFA c s 
	-> [[ Trenner c s ]]
	-> Reporter (Klassen s)
equiv url sigma a tss = do
    inform $ vcat $ map text 
	          [ "Sie sollen die Äquivalenzklassen"
		  , "für diesen Automaten bestimmen:"
		  , url
		  ]
    foldM ( schritt sigma a ) ( start a ) $ zip [0..] $ tss ++ [[]]


start :: NFAC c s 
      => NFA c s -> Klassen s
start a = mkSet [ finals a, states a `minusSet` finals a ]

zerlege :: NFAC c s
	=> Set c -> NFA c s -> [Klassen s]
zerlege sigma a = 
    let f xss = anwende xss $ trenner sigma a xss
    in	fixes f $ start a

-------------------------------------------------------------------





