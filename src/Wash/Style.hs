-- © 2001, 2002 Peter Thiemann

-- with modifications by Johannes Waldmann joe@informatik.uni-leipzig.de
--   $Id$

module Wash.Style (Style(..), using) where

import Prelude hiding ( span, div, head, map )

import Wash.HTMLMonad
import Data.List  (nub )

infixl 5 :^:
infix 9 :=:

data Style 
  = NoStyle
  | String :=: String					    -- primitive style
  | Style  :^: Style					    -- combined style
  | Named String					    -- from style sheet

instance Show Style where
  showsPrec n NoStyle = id
  showsPrec n (s1 :=: s2) = showString s1 . showString ": " . showString s2
  showsPrec n (s1 :^: s2) = shows s1 . showString "; " . shows s2
  showsPrec n (Named s) = id

extractNamed NoStyle   = []
extractNamed (Named n) = [n]
extractNamed (_ :=: _) = []
extractNamed (s1 :^: s2) = 
  nub (extractNamed s1 ++ extractNamed s2)

attach :: (Monad m) => Style -> WithHTML m ()
attach sty = 
  attr "style" (show sty) ##
  case extractNamed sty of
    [n] -> attr "class" n
    _   -> empty

using style elem x = elem (x ## attach style)
