-- | TODO: move to Autolib.*

module Type.Tree 

( peng 
)

where

import qualified Tree as T
import Autolib.TES.Term
import Autolib.Dot.Dotty

instance ( Show v, Show a ) => T.ToTree ( Term v a ) where
    toTree ( Var v ) = T.Node ( show v ) []
    toTree ( Node f args ) = T.Node ( show f ) $ map T.toTree args
