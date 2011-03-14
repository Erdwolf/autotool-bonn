module Haskell.SmallCheck where


import Haskell.Data
import Autolib.ToDoc

import qualified Test.SmallCheck as SC
import Test.SmallCheck ( Serial (..), Series (..) )

import Control.Monad ( guard )

run :: Testable b
    => Instance 
    -> ( a -> b )
    -> a
    -> [[String]]
run i f a = 
    let p = f a
        results = take ( tests_run $ driver i ) $ do
            s <- [ 0 .. ]
            evaluate p s
        failures = take ( failures_shown $ driver i) $ do
            res <- results
            guard $ ok res == Just False
            return $ arguments res
    in  failures


-- this is copied from smallcheck sources,
-- but they don't export it

newtype PR = Prop [Result]
data Result = Result {ok :: Maybe Bool, arguments :: [String]}
    deriving Show


newtype Property = Property (Int -> PR)


class Testable a where
  property :: a -> Int -> PR

instance Testable Bool where
  property b _ = Prop [Result (Just b) []]

instance Testable PR where
  property prop _ = prop

instance (Serial a, Show a, Testable b) => Testable (a->b) where
  property f = f' where Property f' = forAll series f

instance Testable Property where
  property (Property f) d = f d

evaluate :: Testable a => a -> Series Result
evaluate x d = rs where Prop rs = property x d

forAll :: (Show a, Testable b) => Series a -> (a->b) -> Property
forAll xs f = Property $ \d -> Prop $
  [ r{arguments = show x : arguments r}
  | x <- xs d, r <- evaluate (f x) d ]
