module Baum.Draw 

( standard, compact, compact' )

where

import Data.Tree

-- | represents a rectangular array of characters 
-- implementation invariants: 
-- number of contents lines is height
-- all contents lines have length width
-- make sure that computation of height and width
-- for composition of boxes does not depend on their contents, 
-- otherwise choosing best layout will be expensive
data Box =
     Box { height :: Int
             , width  :: Int
             , contents :: [ String ]
             }

instance Show Box where 
    show = unlines . contents

-- | the cost function used (minimized) by the layout algorithm
extension :: Box -> Int
extension b = height b + width b

-- | construct box object that fulfills invariant
make css = 
    let w = maximum $ 0 : map length css
    in  Box
        { height = length css
        , width  = w
        , contents = map ( fill w ) css
        }

empty = make []

-- | fill a line of characters to given length by whitespaces
fill w [] = replicate w ' '
fill w (c : cs) = c : fill (w-1) cs


-- | side by side, aligned at top
beside :: Box -> Box -> Box
beside l r = 
    let h = max ( height l ) ( height r )
        w = width l + width r
        padded b = contents b ++ repeat ( fill ( width b ) "" )
    in  Box { height = h
            , width  = w
            , contents = take h $ zipWith (++) ( padded l ) ( padded r )
            }

-- | put many side by side
besides :: [Box] -> Box
besides = foldr beside empty

-- | above, aligned left
above :: Box -> Box -> Box
above t b = 
    let h = height t + height b 
        w = max (width t) (width b)
        padded x = map ( fill w ) $ contents x
    in  Box { height = h
            , width  = w
            , contents = padded t ++ padded b
            }

-- | put many atop each other
aboves = foldr above empty

-------------------------------------------------------------------------

-- | put a handle on top
handle_top islast b = 
    let top = make [ if islast then "\\" else "+" ++ replicate ( width b ) '-' ]
    in  aboves [ top, make ["|"],  b ]

-- | put a handle on the left
handle_left islast b = 
    let left = if islast then "`" else "+" ++ replicate ( height b ) '|'
        flip cs = make $ map return cs
    in  besides [ flip left , make [ "-" ] , b ]

-- | combine several
link_beside [] = empty
link_beside [b] = b
link_beside bs = besides $ do
    ( k, b ) <- zip [ 1 .. ] bs
    return $ handle_top ( k == length bs ) b

-- | combine several
link_above [] = empty
link_above [b] = b
link_above bs = aboves $ do
    ( k, b ) <- zip [ 1 .. ] bs
    return $ handle_left ( k == length bs ) b

-- | Data.Tree.drawTree behaviour, uses only 
standard :: Tree String -> String
standard = 
    let stand ( Node f [] ) = make [ f ]
        stand ( Node f xs ) = 
            aboves [ make [f] , make ["|"], link_above $ map stand xs ]
    in  show . stand

-- | one of link_beside, link_above
link_best bs = 
    let a = link_above  bs
        b = link_beside bs
    in  if extension a < extension b then a else b


-- | one of link_beside, link_above
link_best' bs = 
    let a = link_above  $ reverse bs
        b = link_beside bs
    in  if extension a < extension b then a else b

-- | replacement for Data.Tree.drawTree, tries to use clever layout
compact :: Tree String -> String
compact = 
    let comp ( Node f [] ) = make [ f ]
        comp ( Node f xs ) = 
            aboves [ make [f] , make ["|"], link_best $ map comp xs ]
    in  show . comp

-- | replacement for Data.Tree.drawTree, tries to use clever layout
-- reverse order of nodes for vertical arrangement (leftmost = lowest)
compact' :: Tree String -> String
compact' = 
    let comp ( Node f [] ) = make [ f ]
        comp ( Node f xs ) = 
            aboves [ make [f] , make ["|"], link_best' $ map comp xs ]
    in  show . comp

-------------------------------------------------------------------------

test = putStrLn $ compact $ f 5

f n = if n < 2 then Node ( show n ) []
      else Node ( show n ) [ f (n-1), f (n-2) ]
