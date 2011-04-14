
module RedBlackTree.Insert ( make_fixed, make_quiz ) where

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Ana
import Autolib.Size

import Inter.Types
import Inter.Quiz

import RedBlackTree.Param

import System.Random
import Data.Typeable

-------------------------------------------------------------------------------

data RedBlackTree = RedBlackTree deriving ( Eq, Show, Read, Typeable )

instance Size Integer where size _ = 1

instance Partial RedBlackTree [ Integer ] String where

  describe RedBlackTree xs = vcat [
      text "Gesucht ist der Rot-Schwarz-Baum der durch das nacheinanderfolgende Einfügen der Werte",
      toDoc xs,
      text "in den Baum entsteht.",
      text "\nEs wird nach den Regeln von Okasaki [1] eingefügt.",
      text "\nDie Invarianten für den Baum sind:",
      text "1) Kein roter Knoten hat einen roten Elter.",
      text "2) Jeder Pfad von der Wurzel zu einem Blatt enthält die gleiche Anzahl schwarzer Knoten.",
      text "\nIm Falle eines roten Kinds mit rotem Elter wird um den Elter zum Großelter hin rotiert.",
      text "Dies ist auch in Abbildung 1 in [1] dargestellt.",
      text "\nAls Lösung ist die Eingabe des Baums als Zeichenkette der Form",
      text "\"('linker Teilbaum' 'Knotenwert und -farbe' 'rechter Teilbaum')\".",
      text "\nBeispiel 1: \"E\"                leerer Rot-Schwarz-Baum.",
      text "Beispiel 2: \"(E 1b E)\"         Baum mit Wurzel des Wertes 1 und der Farbe Schwarz",
      text "                               (r = Rot, b = Schwarz) und leeren Teilbäumen.",
      text "Beispiel 3: \"(E 1b (E 2r E))\"  Baum mit Wurzel des Wertes 1 und der Farbe Schwarz,",
      text "                               leerem linken Teilbaum und rechten Teilbaum mit Wert 2,",
      text "                               Farbe Rot und leeren Teilbäumen.",
      text "\n[1] Chris Okasaki (1999) Red-Black Trees in a Functional Setting.",
      text "    Journal of Functional Programming 9(4):471--477.",
      text "    http://www.eecs.usma.edu/webs/people/okasaki/jfp99.ps"
    ]

  initial RedBlackTree xs = "E"

  total RedBlackTree xs solution = do
    when ( ( show $ rbInsertList xs ) /= solution )
      $ reject $ vcat [
          text "Nein!",
          text "\nDer Rot-Schwarz-Baum, der durch das Einfügen von",
          toDoc xs,
          text "entsteht, ist nicht",
          toDoc solution
        ]
    inform $ text "Ja!"

make_fixed :: Make
make_fixed = direct RedBlackTree [ 1 :: Integer , 3, 5, 7, 2, 4, 8 ]
-- "((E 1b (E 2r E)) 3b (((E 4r E) 5b E) 7r (E 8b E)))"

make_quiz :: Make
make_quiz = quiz RedBlackTree RedBlackTree.Param.example

roll :: Param -> IO [ Integer ]
roll p = sequence $ do
  i <- [ 1 .. anzahl p ]
  return $ do
    cs <- sequence $ replicate ( stellen p ) $ randomRIO ( 0, 9 )
    return $ foldl ( \ a b -> 10 * a + b ) 0 cs

instance Generator RedBlackTree Param [ Integer ] where
  generator _ conf key = do
     xs <- roll conf
     return xs

instance Project RedBlackTree [ Integer ][ Integer ] where
  project _ = id

---------------------------------------

{- RedBlackTree.hs

  A Red-black tree search and insert implementation in Haskell
  after Okasaki [1].
  
  The invariants for this functional programming approach to Red-black
  trees are
  1) No red node has a red parent.
  2) Every path from the root to an empty node contains the same number
     of black nodes.
  
  As Okasaki points out for these invariants to hold it is only
  necessary to eliminate red nodes with red parents. That's nice!
  
  This code is provided ``as is'' without any warranty and with
  credits to Okasaki for his work. It will be used to implement
  a module for autotool, ``a program for checking student exercises
  in theoretical computer science'' [2]. The task is part of a class
  in functional programming at HTWK Leipzig [3].
  
  [1] Chris Okasaki (1999) Red-Black Trees in a Functional Setting.
      Journal of Functional Programming 9(4):471--477.
      http://www.eecs.usma.edu/webs/people/okasaki/jfp99.ps
  [2] http://www.imn.htwk-leipzig.de/~waldmann/autotool/
  [3] http://www.htwk-leipzig.de/

-}

data RbColor = Red | Black
  deriving ( Eq )

data RbTree a = Empty
              | RbTree RbColor ( RbTree a ) a ( RbTree a )

rbElem :: Ord a => a -> RbTree a -> Bool
rbElem x Empty = False
rbElem x ( RbTree _ left y right ) | x <  y = rbElem x left
                                   | x == y = True
                                   | x >  y = rbElem x right

rbInsert :: Ord a => a -> RbTree a -> RbTree a
rbInsert x t = rbDyeNodeBlack ( rbInsert' x t )

-- Equal node values are inserted into the left subtree.
rbInsert' :: Ord a => a -> RbTree a -> RbTree a
rbInsert' x Empty = RbTree Red Empty x Empty
rbInsert' x ( RbTree color left y right )
            | x <= y = rbBalance color ( rbInsert' x left ) y right
            | x >  y = rbBalance color left y ( rbInsert' x right )

rbDyeNodeBlack :: Ord a => RbTree a -> RbTree a
rbDyeNodeBlack ( RbTree _ left x right ) = RbTree Black left x right

{- Black grandparents take responsibility for recoloring their
   red children and grandchildren. The resulting tree might have
   a red root node (--> call to ``rbDyeNodeBlack'' in ``rbInsert''). -}
rbBalance :: Ord a => RbColor -> RbTree a -> a -> RbTree a -> RbTree a
-- 1) Left case in [1, Fig. 1]:
rbBalance Black ( RbTree Red ( RbTree Red a x b ) y c ) z d
  = RbTree Red ( RbTree Black a x b ) y ( RbTree Black c z d )
-- 2) Top case in [1, Fig. 1]:
rbBalance Black ( RbTree Red a x ( RbTree Red b y c ) ) z d
  = RbTree Red ( RbTree Black a x b ) y ( RbTree Black c z d )
-- 3) Bottom case in [1, Fig. 1]:
rbBalance Black a x ( RbTree Red ( RbTree Red b y c ) z d )
  = RbTree Red ( RbTree Black a x b ) y ( RbTree Black c z d )
-- 4) Right case in [1, Fig. 1]:
rbBalance Black a x ( RbTree Red b y ( RbTree Red c z d ) )
  = RbTree Red ( RbTree Black a x b ) y ( RbTree Black c z d )
-- 5) Any other case (no balancing necessary):
rbBalance color left x right = RbTree color left x right
-- Note that the right hand sides of cases 1--4 are the same.

{- Insert a list of values into an empty RbTree -}
rbInsertList :: Ord a => [a] -> RbTree a
rbInsertList xs = rbInsertList' xs Empty

rbInsertList' :: Ord a => [a] -> RbTree a -> RbTree a
rbInsertList' []         Empty                   = Empty
rbInsertList' []         rb @ ( RbTree _ _ _ _ ) = rb
rbInsertList' ( x : xs ) Empty                   = rbInsertList' xs ( rbInsert x Empty )
rbInsertList' ( x : xs ) rb @ ( RbTree _ _ _ _ ) = rbInsertList' xs ( rbInsert x rb )

{- Show instances. -}
instance Show RbColor where
  show Red = "r"
  show Black = "b"

instance Show a => Show ( RbTree a ) where
  show Empty
    = "E"
  show ( RbTree color left x right )
    = "(" ++ show left ++ " " ++ show x ++ show color
                       ++ " " ++ show right ++ ")"
