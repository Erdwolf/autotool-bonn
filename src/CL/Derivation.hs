module CL.Derivation where

import Autolib.Schichten

import Data.Set ( Set )
import qualified Data.Set as S
import Control.Monad ( guard )

-- | comparison is by end-of-derivation only
-- this element actually is in the head
-- (that is, order ist reversed, for easier access)
data Derivation a = Derivation { contents :: [a] }
instance Eq a => Eq (Derivation a) where
    d1 == d2 = top d1 == top d2
instance Ord a => Ord (Derivation a) where
    compare d1 d2 = compare ( top d1) (top d2)

extend :: a -> Derivation a -> Derivation a
extend x ( Derivation xs ) = Derivation (x : xs)
top :: Derivation a -> a
top ( Derivation (x:xs)) = x

unit :: a -> Derivation a
unit x = Derivation [x]

paths_from_to :: Ord a 
      => ( a -> [ a ] ) -- ^ successor
      -> Int -- ^ max path length
      -> a -- ^ start
      -> a -- ^ end
      -> [ [a] ]
paths_from_to next depth start end = do
    let f d = S.fromList $ do x <- next $ top d ; return $ extend x d
    ds <- take depth $ schichten f $ unit start
    d <- S.toList ds
    guard $ top d == end
    return $ reverse $ contents d

paths_from next depth start = do
    let f d = S.fromList $ do x <- next $ top d ; return $ extend x d
    ds <- take depth $ schichten f $ unit start
    d <- S.toList ds
    return $ reverse $ contents d


