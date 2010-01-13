{-# language TemplateHaskell, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module Graph.Col.Greedy.Central where

import Graph.Util
import Graph.Color
import Autolib.Graph.Basic ( circle )

import Autolib.Graph.Ops ( gmap )
import Autolib.Dot ( peng, Layout_Program (..) )

import qualified Autolib.Multilingual as M
import qualified Text.PrettyPrint.HughesPJ as T

import Inter.Types
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Hash
import Autolib.Size
import Autolib.FiniteMap
import qualified Challenger as C

import qualified Autolib.Reporter.Set
import Data.Typeable
import Data.List ( sort )
import Data.Maybe ( maybeToList )

data Greedy_Col = Greedy_Col 
     deriving ( Eq, Ord, Show, Read, Typeable )

data GraphC a => Instance a = Instance
        { max_nodes :: Int
        , min_difference :: Int
        }
    deriving Typeable
$(derives [makeReader,makeToDoc] [''Instance])

example_instance :: Instance Int
example_instance = Instance
    { max_nodes = 20
    , min_difference = 2
    }

data GraphC a => Solution a = Solution
    { graph :: Graph a
    , good_sequence :: [a]
    , bad_sequence :: [a]
    }
    deriving Typeable
$(derives [makeReader,makeToDoc] [''Solution])

instance GraphC a => Size ( Solution a ) where
    size s = size $ graph s

example_solution :: Solution Int
example_solution = Solution
    { graph = circle [ 1 .. 6 ]
    , good_sequence = [ 1 .. 6 ]
    , bad_sequence = [ 1,4,2,5,3,6 ]
    }

instance C.Partial Greedy_Col ( Instance Int ) ( Solution Int ) where

    describe p i = vcat
        [ text "Gesucht ist ein Graph mit höchstens" <+> toDoc ( max_nodes i ) <+> text "Knoten"
        , text "und zwei Knotenreihenfolgen,"
        , text "für die sich die bei Greedy-Färbung benötigte Farbzahl"
        , text "um wenigstens" <+> toDoc ( min_difference i ) <+> text "unterscheidet."
        ]

    initial p i = example_solution

    partial p i s = do
        let g = graph s
        when ( size g > max_nodes i ) $ reject $ text "Der Graph ist zu groß."
        inform $ vcat [ text "Ihr Graph ist" , nest 4 $ toDoc g ]
        peng $ g { layout_program = Dot
                 , layout_hints = [ "-Nshape=ellipse" ]
                 }
        validate g
        must_be_permutation ( "Knoten", lknoten g) ("good_sequence",  good_sequence s )
        must_be_permutation ( "Knoten", lknoten g) ("bad_sequence ",  bad_sequence  s )

    total p i s = do
        good <- greedy_colour ( graph s ) ( good_sequence s ) 
        bad  <- greedy_colour ( graph s ) ( bad_sequence  s )
        let diff = bad - good
        inform $ text "Die Differenz der Farbzahlen ist" <+> toDoc diff
        when ( diff < min_difference i ) $ reject 
             $ text "Dieser Wert ist zu klein."

greedy_colour g xs = do
    inform $ text "Greedy-Färbung für Reihenfolge" </> toDoc xs
    let handle [] col = do
            inform $ text "Färbung" </> toDoc col
            let c = size $ mkSet $ eltsFM col
            inform $ text "Anzahl der benutzten Farben:" <+> toDoc c
            return c
        handle (x:xs) col = do
            let yfs = do
                   y <- lnachbarn g x
                   return ( y, lookupFM col y )
            inform $ vcat
                [ text "Knoten:" <+> toDoc x
                , text "Nachbarn, ggf. mit Farben:" <+> text ( show  yfs )
                ]
            let used = concat $ map maybeToList $ map snd yfs
                free = filter ( \ f -> not ( f `elem` used ) ) 
                              [ minBound :: Color .. maxBound ]
            f <- case free of
                f : _ -> do
                    inform $ text "kleinste freie Farbe:" <+> toDoc f
                    return f
                [] -> reject $ text "alle Farben verbraucht (Knotengrad zu hoch?)"
            handle xs $ addToFM col x f
    nested 4 $ handle xs emptyFM
    

must_be_permutation (mx, xs) (my, ys) = do
    case diff ( sort xs ) ( sort ys ) of
        d : _ -> reject $ vcat
            [ text mx <+> equals <+> toDoc xs
            , text "ist keine Permutation von" 
            , text my <+> equals <+> toDoc ys
            , text "fehlerhaftes Element:" <+> toDoc d
            ]
        [] -> return ()

diff [] [] = []
diff (x:xs) (y:ys) =
    if x == y then diff xs ys else [ min x y ]
diff (x:xs) [] = [x]
diff [] (y:ys) = [y]


make :: Make
make = direct Greedy_Col example_instance



