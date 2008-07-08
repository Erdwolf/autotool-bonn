{-# language TypeSynonymInstances #-}

-- | derivation trees

module Grammatik.CF.Baum where

--  $Id$

import Grammatik.Type
import Autolib.ToDoc

import Autolib.TES.Type
import Autolib.TES.Position
import Autolib.TES.Draw
import Autolib.TES.Identifier
import Autolib.Schichten
import Autolib.FiniteMap

import Control.Monad ( guard )

-- | Boolean tag is true for Variables that have been replaced by Epsilon
type Baum = Term Char ( Char, Bool )

instance ToDoc Baum where
    toDoc b = draw $ patch b

patch :: Baum -> Term Identifier Identifier
patch ( Var v ) = 
    Var $ mknullary [ v ]
patch ( Node ( f, tag ) args ) = 
    Node ( mknullary [ f ] ) 
	$ if tag && null args 
	  then [ Var $ mknullary $ show ""  ]
	  else map patch args
 
next :: Grammatik -> Baum -> [ Baum ]
next g b = do
    ( p, Var v ) <- positions b
    ( l, r ) <- rules g
    guard $ l == [v]
    let t = Node ( v, True ) $ do
           x <- r
           return $ if x `elementOf` variablen g
		    then Var x
		    else Node ( x, False ) []
    return $ poke b ( p, t )


-- | lazy list of all derivation trees
baums :: Grammatik -> [ Baum ]
baums g = do
    ts <- schichten ( mkSet . next g ) ( Var $ startsymbol g )
    setToList ts

-- | reading along the leaves 
yield :: Baum -> String
yield = tfold ( \ v -> [ v ] )
	      ( \ (f, tag) xs -> if null xs && not tag 
		                 then [f] else concat xs )


duplicates :: Ord b 
	     => (a -> b)
	     -> [a]
	     -> [(a,a)]
duplicates f xs = 
    let handle fm [] = []
        handle fm (x : xs) = 
          let y = f x
          in  case lookupFM fm y of
                   Nothing -> handle (addToFM fm y x) xs
		   Just x' -> (x, x') : handle  fm xs
    in  handle emptyFM xs

-- | find all pairs of different derivation trees with equal yields,
-- among the given number of trees
ambigs :: Int 
       -> Grammatik
       -> [ ( Baum, Baum) ]
ambigs cut g = duplicates yield
	     $ take cut 
	     $ baums g




ex :: Grammatik
ex = Grammatik { terminale = mkSet "ab", variablen = mkSet "KLGAS"
	  , start = 'K'
	  , regeln = mkSet [ ( "K", "G" ), ("K","LA")
                          , ("S", "a"), ("S", "b")
                          ,( "A", "S" ), ("A", "SA")
                          , ( "L", "b" ), ("L", "aLL") 
                          , ("G", ""), ("G", "aG"), ("G", "aGbG")
                    ]
	  }
