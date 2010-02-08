module KnapsackFraction.Central 

( KnapsackFraction
, make_fixed 
, make_quiz 
)

where

import KnapsackFraction.Solve ( packs )
import KnapsackFraction.Param

import Challenger.Partial
import Inter.Types
import Inter.Quiz

import Autolib.ToDoc
import Autolib.Set
import Autolib.FiniteMap
import Autolib.Reporter
import Autolib.Reporter.Set ( eq )
import Autolib.Size ( Size , size )

import Data.Typeable ( Typeable )
import Data.Ratio ( (%) )

import System.Random ( randomRIO )

-------------------------------------------------------------------------------

data KnapsackFraction = KnapsackFraction deriving ( Eq, Typeable, Show, Read )

fm_fun :: FiniteMap Objekt a -> Objekt -> a
fm_fun fm = lookupWithDefaultFM fm (error "data incomplete!?")

count :: (Objekt -> Integer) -> FiniteMap Objekt Rational -> Rational
count f = foldFM ( \ x r v -> v + r * (f x % 1) ) 0

instance OrderScore KnapsackFraction where
    scoringOrder _ = None

instance Size Pack where size = const 1

instance Partial KnapsackFraction Inp Pack where

    describe KnapsackFraction inp = vcat
        [ text "Lösen Sie das Problem Bruchteilrucksack für die Objekte"
	, nest 4 $ toDoc $ objekte inp
	, text "mit den Gewichten"
	, nest 4 $ toDoc $ gewichte inp
	, text "und den Werten"
	, nest 4 $ toDoc $ werte inp
        , text "Der Rucksack hat eine Kapazität von"
	, nest 4 $ toDoc $ kapazitaet inp
	, text "Sie sollen auch den Gesamtwert Ihrer Packung angeben."
	]

    initial KnapsackFraction inp = 
	let packFM = listToFM $ do
		     (x,i) <- zip (setToList $ objekte inp) [(0::Integer)..]
		     return ( x , 1 % (max 1 $ mod i 3) )
        in Pack (count (fm_fun $ werte inp) packFM) packFM

    partial KnapsackFraction inp (Pack value packFM) = do

        eq ( text "vorhandene Objekte" , objekte inp )
	   ( text "Objekte in Ihrer Packung" , mkSet $ keysFM packFM )

        inform $ text "Sind alle Bruchteile zwischen 0 und 1?"

        let badFM = filterFM ( \ _ v -> or [ v < 0 , v > 1 ] ) packFM

        when ( sizeFM badFM > 0 ) $ reject $ vcat
	     [ text "Nein. Diese Bruchteile sind nicht möglich:"
	     , nest 4 $ toDoc badFM
	     ]

        inform $ text "Ja."

        inform $ vcat 
	       [ text "Stimmen der von Ihnen berechnete Gesamtwert"
	       , nest 4 $ toDoc value
	       , text "und der tatsächliche Gesamtwert Ihrer Packung überein?"
	       ]

        let real_value = count (fm_fun $ werte inp) packFM

        when ( value /= real_value ) $ reject $ text "Nein."

        inform $ text "Ja."

    total KnapsackFraction inp (Pack value packFM) = do

        let weight = count (fm_fun $ gewichte inp) packFM

        inform $ vcat 
	       [ text "Überschreitet das Gesamtgewicht Ihrer Einsendung"
	       , nest 4 $ toDoc weight
	       , text "die Kapazität des Rucksacks?"
	       ]

        when ( weight > (kapazitaet inp % 1) ) $ reject $ text "Ja."

        inform $ text "Nein."

        inform $ text "Ist der Gesamtwert maximal?"

        when ( value < optimaler_wert inp ) $ reject 
		 $ text "Nein. Es gibt eine Packung mit höherem Wert!"

        inform $ text "Ja."

make_fixed :: Make
make_fixed = direct KnapsackFraction inp0

instance Generator KnapsackFraction Param (Inp,Pack) where
    generator _ conf _ = do

        let os = take (anzahl conf) $ enumFrom A

        let rands = sequence . replicate (anzahl conf) . randomRIO

        gs <- rands $ gewicht conf
	vs <- rands $ wert    conf

        c <- randomRIO $ kapazitaet0 conf

        let gsFM = listToFM $ zip os gs
	let vsFM = listToFM $ zip os vs

        let ( (opt,p) : _ ) = packs os c (fm_fun gsFM) (fm_fun vsFM)

        return ( Inp (mkSet os) opt c gsFM vsFM
	       , Pack opt (listToFM p)
	       )

instance Project KnapsackFraction (Inp,Pack) Inp where project _ = fst

make_quiz :: Make
make_quiz = quiz KnapsackFraction p0
