{-| Quiz: zweistellige primitiv rekursive Funktion raten

  Zum Beispiel:

  @quiz "FUN" "QUIZ" 12 10@

@
 |             0    1    2    3    4    5    6    7    8    9   10
 |---------------------------------------------------------------
 |   0    |    0    1    2    3    4    5    6    7    8    9   10
 |   1    |    0    0    1    2    3    4    5    6    7    8    9
 |   2    |    1    1    0    1    2    3    4    5    6    7    8
 |   3    |    0    0    2    0    1    2    3    4    5    6    7
 |   4    |    3    3    1    3    0    1    2    3    4    5    6
 |   5    |    2    2    0    2    4    0    1    2    3    4    5
 |   6    |    1    1    5    1    3    5    0    1    2    3    4
 |   7    |    0    0    4    0    2    4    6    0    1    2    3
 |   8    |    7    7    3    7    1    3    5    7    0    1    2
 |   9    |    6    6    2    6    0    2    4    6    8    0    1
 |   10   |    5    5    1    5    9    1    3    5    7    9    0
@
-}
module Fun.Quiz where

--   $Id$

import Fun.Quiz.Type
import Fun.Type
import Fun.Table
import Fun.Examples
import Fun.Check
import Fun.Create
import qualified RAM.Builtin


import Inter.Types
import Challenger.Partial
import Autolib.Informed


-- import Autolib.Datei
-- import Autolib.Cache
-- import Autolib.Seed

import Data.Array
import Autolib.Reporter
import Autolib.ToDoc


instance Partial Quiz Tafel Fun where
    --  Anfangsbeispiel
    initial p i   = Fun.Examples.plus
    -- Partiell Korrekt 
    partial p i b = do           
          check_builtins RAM.Builtin.every b
          check_arity 2 b
    --  Total Korrekt
    total   p i b = do
          inform $ text "Die Wertetabelle Ihrer Funktion ist:"
          let ((0,0), xy) = bounds i
          let t = tabulate2 b xy
          inform $ nest 4 $ rollout $ frame2 t
          -- Unterschiede berechnen
          let diffs = do
                  xy <- indices i
                  guard $ i ! xy /= t ! xy
                  return (xy, i!xy, t!xy)
          -- Bei Unterschieden -> Differenz ausgeben
          when ( not $ null diffs ) $ do
               inform $ text "Die Tabellen stimmen nicht überein:"
               reject $ nest 4 $ toDoc diffs
          -- Sehr schoen: richtig Loesung
          inform $ text "Die Tabellen stimmen überein."
    --  Aufgabe beschreiben
    describe p i = 
            vcat [ text "Konstruieren Sie eine zweistellige primitiv rekursive Funktion"
                 , text "mit dieser Wertetabelle:"
                 , rollout $ frame2 i
                 ]

-- | Konstruktor
quiz :: String -- ^ Aufgabe
     -> String -- ^ Version
     -> Int    -- ^ Größe der Funktion
     -> Int    -- ^ Tabellengröße
     -> Var Quiz Tafel Fun 
quiz auf ver s t =  
    Var { problem = Quiz
        , aufgabe = auf
        , version = ver
          -- erzeugt cached Version der Instanz (o. ä.)
          -- key :: Matrikel -> IO Key
        , key = \ mat -> return mat
          -- holt tatsächliche Instanz
          -- gen :: Key -> IO ( Reporter i )
        , gen = \ key -> do
          seed $ read key
          ( f, tab ) <- cache (  Datei { pfad = [ "autotool", "cache", auf, ver ]
                                       , name = key , extension = "cache" 
                                       }
                                       ) ( nontrivial s t )
          return $ do
           inform $ vcat
                [ text "Gesucht ist eine zweistellige primitiv rekursive Funktion"
                , text "mit folgender Wertetabelle:"
                , nest 4 $ rollout $ frame2 tab
                ]
           return tab
        }

