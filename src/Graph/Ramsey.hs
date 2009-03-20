{-# language UndecidableInstances #-}

module Graph.Ramsey where

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

data Ramsey = Ramsey deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Verify Ramsey [Int] where
    verify p i = do
        when (  null i ) 
             $ reject 
             $ text "Die Liste der x_i darf nicht leer sein."
        when ( any ( <= 0 ) i ) 
             $ reject 
             $ text "Alle x_i sollen positiv sein."

instance C.Partial Ramsey [Int] ( Int, FiniteMap ( Kante Int ) Int ) where

    describe p i = vcat $
        [ text "Gesucht ist eine Kantenfärbung eines K_n"
	, hsep [ text "mit", toDoc ( length i ), text "Farben" ]
        , text "ohne einfarbige K_x_i der Farbe i"
        , hsep [ text "für [ x_1, .. ]", equals, toDoc i ]
	] 

    initial p i = 
        let n = sum i
        in  ( n
            , listToFM $ zip ( lkanten $ clique $ mkSet [ 1 .. n ] )
                       $ concat $ repeat [ 1 .. length i ]
            )

    partial p i (n, f) = do
        let missing = kanten ( clique $ mkSet [ 1 .. n ] ) 
                 `minusSet` mkSet ( keysFM f ) 
        when ( not $ isEmptySet missing ) $ reject $ vcat
             [ text "diese Kanten sind nicht gefärbt:"
             , nest 4 $ toDoc missing
             ]
        let fehlfarben = filter ( \ (k,f) -> not ( 1 <= f && f <= length i ) )
                       $ fmToList f
        when ( not $ null fehlfarben ) $ reject $ vcat
             [ text  "diese Kantenfarben sind nicht erlaubt:"
             , nest 4 $ toDoc fehlfarben
             ]
    
    total p i (n, f) = do
        sequence_ $ do 
            (c, x_c) <- zip [ 1.. ] i
            return $ sequence $ do
                nodes <- teilmengen x_c $ mkSet [ 1 .. n ]
                return $ do
                    let non_c = do
                            k <- lkanten $ clique nodes
                            d <- maybeToList $ lookupFM f k
                            guard $ c /= d
                            return ()
                    when ( null non_c ) $ reject $ vcat
                        [ text "Der Teilgraph mit dieser Knotenmenge"
                        , nest 4 $ toDoc nodes
                        , text "ist einfarbig mit Farbe" <+> toDoc c
                        ]

instance C.Measure Ramsey [Int] ( Int , FiniteMap (Kante Int) Int ) where 
    measure p i ( n, f ) = fromIntegral n

make :: Make
make = direct Ramsey [ 4, 4 :: Int ]




