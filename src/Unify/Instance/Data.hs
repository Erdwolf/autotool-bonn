{-# LANGUAGE DeriveDataTypeable, GADTs, MultiParamTypeClasses, FlexibleInstances, TemplateHaskell, TemplateHaskell #-}

module Unify.Instance.Data where

import Autolib.TES.Term
import Autolib.TES.Identifier
import Autolib.FiniteMap
import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable
import Data.Char (isUpper)

class    ( Symbol v, Symbol c, Typeable v, Typeable c ) => InstanceC v c
instance ( Symbol v, Symbol c, Typeable v, Typeable c ) => InstanceC v c

data InstanceC v c => Instance v c = Instance { wildcard :: c
              , left :: Term v c
              , right :: Term v c
              , unifier :: FiniteMap v ( Term v c )
              }
    deriving ( Typeable )


type TII = Term Identifier Identifier

patch_variables :: TII -> TII
patch_variables ( Node f xs ) = 
    if isvariable f
    then if null xs then Var f else error "Unify.Main.patch_variable: with arguments?"
    else Node f $ map patch_variables xs

isvariable :: Identifier -> Bool
isvariable i = isUpper $ head $ show i

$(derives [makeReader, makeToDoc] [''Instance])
-- {-! for Instance derive: Reader, ToDoc !-}

example :: Instance Identifier Identifier
example = Instance
    { wildcard = read "undefined"
    , left = patch_variables $ read "f (f (Y, undefined), f (f (Z, f (a, undefined)), undefined))"
    , right = patch_variables $ read "f (f (f (X, a), X), undefined)"
    , unifier = mapFM ( \ k v -> patch_variables v ) 
              $ listToFM $ read "[ ( X , f (Z, f (a, a)) ) , ( Y , f (f (Z, f (a, a)), a) ) ]"
    }


-- local variables:
-- mode: haskell
-- end:
