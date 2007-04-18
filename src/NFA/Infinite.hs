module NFA.Infinite where

import Autolib.NFA
import Autolib.Set

import qualified Autolib.Relation as R
import Control.Monad ( guard )

-- | set of states that produce infinite language
infinite_states :: NFAC c s 
                => NFA c s -> Set s
infinite_states a =
    let b = Autolib.NFA.trim a
        r = R.make $ do 
                (p,c,q) <- unCollect $ trans a
                return (p,q)
        t = R.trans r
        loops = mkSet $ do 
             (p,q) <- R.pairs t
             guard $ p == q
             return p
    in  R.simages ( R.reflex_trans $ R.inverse r ) loops

is_infinite ::  NFAC c s 
                => NFA c s -> Bool
is_infinite a = not $ isEmptySet 
    $ intersect ( infinite_states a ) ( starts a )

-- | set of states q such that L(start -> q) is infinite
pre_infinite_states :: NFAC c s 
                => NFA c s -> Set s
pre_infinite_states a =
    let b = Autolib.NFA.trim a
        r = R.make $ do 
                (p,c,q) <- unCollect $ trans a
                return (p,q)
        t = R.trans r
        loops = mkSet $ do 
             (p,q) <- R.pairs t
             guard $ p == q
             return p
    in  R.simages ( R.reflex_trans $  r ) loops


