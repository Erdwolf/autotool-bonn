-- | TODO: move to Autolib.*

module Type.Tree 

( peng 
)

where

import qualified Tree as T
import Autolib.TES.Term
import Autolib.Dot.Dotty

instance ( Show v ) => T.ToTree ( Term v ) where
    toTree ( Var v ) = T.Node ( show v ) []
    toTree ( Node f args ) = T.Node ( show f ) $ map T.toTree args
