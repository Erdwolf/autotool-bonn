module Faktor.Euklid (
       make_fixed
     , make_quiz
) where

import Faktor.Euklid.Param

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Util.Zufall

import Inter.Types
import Inter.Quiz

import System.Random
import Data.Typeable

data Euklid = Euklid deriving ( Show, Typeable )

instance Partial Euklid ( Integer, Integer ) ( Integer, Integer ) where
    describe Euklid (a, b) = vcat
        [ text "Gegeben ist das Zahlenpaar (a, b)" <+> equals <+> toDoc (a, b)
        , text "Gesucht ist ein Paar (c, d) von Zahlen"
        , text "mit der Eigenschaft  a * c + b * d = ggT (a, b)"
        ]
    initial Euklid (a, b) = (11, 13)
    partial Euklid (a, b) (c, d) = do
        let g = a * c + b * d
        inform $ text "a * c + b * d" <+> equals <+> toDoc g
        when ( 0 /= g ) $ do
            teilt g a
            teilt g b
    total Euklid (a, b) (c, d) = do
        let g = a * c + b * d
        inform $ fsep 
               [ text "Ist", toDoc g, text "größter gemeinsamer Teiler"
               , text "von", toDoc a, text "und", toDoc b, text "?"
               ]
        if  ggt (a, b) == g
            then inform $ text "Ja."
            else reject $ text "Nein."

instance Measure Euklid  ( Integer, Integer ) ( Integer, Integer ) where
    measure Euklid ( a, b ) ( c, d ) = abs c + abs d
        
teilt :: Integer -> Integer -> Reporter ()
teilt t x = do
    inform $ fsep 
           [ text "Ist", toDoc t, text "ein Teiler von", toDoc x , text "?" ]
    let (q, r) = divMod x t
        info = parens 
             $ fsep [ text "Quotient:", toDoc q, text "Rest:", toDoc r ]
    nested 4 $ if 0 == r
       then inform $ text "Ja." <+> info
       else reject $ text "Nein." <+> info


instance Generator Euklid Param ( Integer, Integer ) where
    generator _ conf key = 
        do a <- randomRIO ( von conf , bis conf )
           b <- randomRIO ( von conf , bis conf )
           return (a, b)
      `repeat_until` \ (a, b) -> ggt (a, b) <= max_ggt conf

instance Project Euklid ( Integer, Integer ) ( Integer, Integer ) where
    project _ = id


make_quiz :: Make
make_quiz = quiz Euklid Faktor.Euklid.Param.p

make_fixed :: Make
make_fixed = direct Euklid ( 744 :: Integer, 531 :: Integer )


-- | euklid (a, b) = (c, d)  implies  a*c + b*d = ggt a b
euklid :: ( Integer, Integer ) -> ( Integer, Integer )
euklid ( a, b ) | b < 0 =
    let ( c, d ) = euklid (a, negate b)
    in  ( c, negate d )
euklid ( a, 0 ) = ( 1, 0 )
euklid ( a, b ) =
    let ( q, r ) = divMod a b
        ( c, d ) = euklid ( b, r )
    in  ( d, c - q * d )

ggt :: ( Integer, Integer ) -> Integer
ggt (a, b) = 
    let (c, d) = euklid (a, b) 
    in  a * c + b * d         
