{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleInstances #-}

module Rewriting.TRS 
( module Rewriting.TRS
, module Autolib.Symbol
, module Autolib.TES.Term
, module Autolib.TES.Position
, module Autolib.TES.Rule
, module Autolib.TES.Identifier
) where

import qualified Rewriting.TRS.Raw as Raw

import Autolib.Symbol
import Autolib.TES.Term
import Autolib.TES.Position
import Autolib.TES.Rule
import Autolib.TES.Identifier
import qualified Autolib.TES

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data ( Symbol c, Symbol v ) => TRS v c = 
     TRS { variablen :: [ v ]
         , regeln :: [ Rule ( Term v c ) ]
         }
    deriving ( Eq, Ord, Typeable )

instance ( Symbol c, Symbol v ) => ToDoc ( TRS v c ) where
    toDoc trs = toDoc 
              $ Raw.TRS 
              { Raw.variablen = variablen trs
              , Raw.regeln    = regeln    trs
              }

instance ( Symbol c ) => Reader ( TRS c c ) where
    reader = do
        trs <- reader
        patch trs

-- | upon reading, the parser does not know what is a variable
-- so the system has to be patched
patch :: ( Symbol c ) 
      => Raw.TRS c c 
      -> Parser ( TRS c c )
patch trs = do
    let handle t @ ( Node f xs ) =
           if f `elem` Raw.variablen trs 
              then if null xs 
                   then return $ Var f
                   else fail 
                        $ show
                        $ text "Variable darf keine Argumente haben:" 
                               <+> toDoc t
              else do
                   ys <- mapM handle xs
                   return $ Node f ys
    rules <- sequence $ do
        rule <- Raw.regeln trs
        return $ do
            ll <- handle $ lhs rule
            rr <- handle $ rhs rule
            return $ rule { lhs = ll, rhs = rr }
    return $ TRS { variablen = Raw.variablen trs
                 , regeln = rules 
                 }   

example :: TRS Identifier Identifier
example = read "TRS { variablen = [x, y] , regeln = [ f(x,y) -> f(y,x) ] }"


pack :: ( Symbol c, Symbol v ) 
     => TRS v c -> Autolib.TES.RS c ( Term v c )
pack trs = Autolib.TES.from_strict_rules False 
         $ do r <- regeln trs ; return ( lhs r, rhs r )
