module NFA.ToExp where

import Autolib.NFA.Type
import Autolib.Exp.Inter
import Autolib.Exp.Type
import Autolib.Size
import Autolib.Symbol

import Data.Array
import Control.Monad ( guard )

a :: NFA Char Int
a = inter std $ read "a^2^* $ b^2^* $ c^2^*"

toExp :: NFAC c Int => NFA c Int -> RX c
toExp a = 
    let n = size a
        bnd = ((1,1,0), (n,n,n))
        t = array bnd $ do
            pqr @ (p,q,r) <- range bnd
            return ( pqr
                   , if r > 0
                     then sUnion ( t!(p,q,r-1) ) 
                            ( foldr1 sDot [ t!(p,r,r-1) , sPowerStar(t!(r,r,r-1)), t!(r,q,r-1)])
                     else foldr sUnion ( if p == q then Ref "Eps" else Ref "Empty" ) $ do
                          ( p', c, q' ) <- unCollect $ trans a
                          guard $ p == p' && q == q'
                          return $ Letter c
                          
                   )
    in  foldr sUnion ( Ref "Empty" ) $ do
            p <- lstarts a
            q <- lfinals a
            return $ t!(p,q,n)

sUnion x y = simplify $ Union x y
sDot   x y = simplify $ Dot x y
sPowerStar x = simplify $ PowerStar x

-- | assume arguments are already simplified
simplify :: Symbol c =>  RX c -> RX c

simplify ( Union x y ) | (0 == ) $ size $ inter std $ Difference x y = y
simplify ( Union x y ) | (0 == ) $ size $ inter std $ Difference y x = x

simplify ( Union ( Ref "Empty" ) y ) = simplify y
simplify ( Union x ( Ref "Empty" ) ) = simplify x
simplify ( Union ( Union ( Ref "Eps" ) y ) z ) = sUnion ( Ref "Eps" ) $ sUnion y z
simplify ( Union x ( Union y ( Ref "Eps" ) ) ) = sUnion ( Ref "Eps" ) $ sUnion x y
simplify ( Union x y ) = Union x y

simplify ( PowerStar ( Ref "Empty" ) ) = Ref "Eps"
simplify ( PowerStar ( Ref "Eps" ) ) = Ref "Eps"
simplify ( PowerStar ( Union ( Ref "Eps" ) y ) ) = simplify ( PowerStar y )
simplify ( PowerStar ( Union x ( Ref "Eps" ) ) ) = simplify ( PowerStar x )
simplify ( PowerStar x ) = PowerStar x

simplify ( Dot ( Ref "Empty" ) y ) = Ref "Empty"
simplify ( Dot x ( Ref "Empty" ) ) = Ref "Empty"
simplify ( Dot ( Ref "Eps" ) y ) = y
simplify ( Dot x ( Ref "Eps" ) ) = x
simplify ( Dot x y ) = Dot x y

simplify sonst = sonst


expand :: Symbol c => RX c -> RX c
expand ( Union x y ) = sUnion ( expand x ) ( expand y )
expand ( Dot ( Union x y ) z ) = sUnion ( expand $ sDot x z ) ( expand $ sDot y z )
expand ( Dot x ( Union y z ) ) = sUnion ( expand $ sDot x y ) ( expand $ sDot x z )
expand ( PowerStar x ) = sPowerStar ( expand x )
expand sonst = sonst

