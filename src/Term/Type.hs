module Term.Type where

-- -- $Id$

import ToDoc
import FiniteMap


data Term a = Term { symbol :: a
		   , children :: [ Term a ]
		   }
	    deriving ( Eq, Ord )


type Position = [ Int ]

type Rule a = ( VTerm a, VTerm a )
type TRS  a = [ Rule a ]

type Variable = String
type VTerm a = Term (Either Variable a)

isvar :: Term (Either Variable a) -> Bool
isvar = isLeft . symbol

isLeft (Left x) = True ; isLeft (Right _) = False

type Substitution a = FiniteMap Variable a 

---------------------------------------------------------------------------

instance ToDoc a => ToDoc (Term a) where
    toDoc x = if ( null $ children x ) then toDoc $ symbol x
	      else parens $ toDoc (symbol x) <+> fsep ( map toDoc $ children x)

instance ToDoc a => Show (Term a) where
    show = render . toDoc

instance Functor Term where
    fmap f x = Term { symbol = f $ symbol x
		    , children = map (fmap f) $ children x
		    }

------------------------------------------------------------------------------

peek :: Term a -> Position -> Term a
peek x [] = x
peek x (p : ps) = peek (children x !! p) ps

poke :: Term a -> Position -> Term a -> Term a
poke x [] y = y
poke x (p : ps) y = 
    let (here, c : there) = splitAt p $ children x
    in  x { children = here ++ poke c ps y : there }

positions :: Term a -> [ Position ]
positions x = [] : do
    (p, c) <- zip [0..] $ children x
    ps <- positions c
    return $ p : ps

subterms :: Term a -> [ Term a ]
subterms x = map (peek x) $ positions x

