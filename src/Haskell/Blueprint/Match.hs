-- | (c) Bertram Felgenhauer, 2011

{-# LANGUAGE Rank2Types, NoMonoLocalBinds #-}

module Haskell.Blueprint.Match where

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Data.Generics
import Control.Applicative
import Control.Monad
import Control.Monad.State

data Result a = Continue | Fail SrcLoc | Ok a
    deriving (Eq, Ord, Show)

newtype M a = M { runM :: State SrcLoc (Result a) }

instance Monad M where
    return = M . return . Ok
    a >>= b = M $ do
        a' <- runM a
        case a' of
            Ok a'' -> runM (b a'')
            Fail loc -> return $ Fail loc
            Continue -> return $ Continue

instance Functor M where
    fmap = liftM

instance Applicative M where
    pure = return
    (<*>) = ap

instance MonadPlus M where
    mzero = M $ return Continue
    a `mplus` b = M $ do
        a' <- runM a
        case a' of
            Continue -> runM b
            Fail _   -> do
                b' <- runM b
                case b' of
                  Ok _ -> return b'
                  _    -> return a' -- Propagate failure information
            _ -> return a'



-- | test whether m1 is a suitable blueprint for module 2
-- in case of an error, the returned srcloc gives the mismatching
-- position in the blueprint
-- test :: String -> String -> Result Module
test m1' m2' = let
    -- first: parse using haskell-src.
    -- ParseOk m1' = parseModule m1
    -- ParseOk m2' = parseModule m2

    -- fail, returning most recently seen location
    failLoc :: M a
    failLoc = M $ get >>= return . Fail

    continue :: M a
    continue = M $ return Continue

    -- match reference to "undefined"
    matchUndef (Just (Var (UnQual (Ident "undefined")))) = True
    matchUndef _ = False

    -- match locations: store blueprint location and ignore the other one
    matchLoc :: Maybe SrcLoc -> Maybe SrcLoc -> M ()
    matchLoc (Just loc) Just{} = M (put loc >> return (Ok ()))
    matchLoc _ _ = continue

    -- check whether given declaration binds the function 'name'.
    matchBind :: Name -> Decl -> Bool
    matchBind name (PatBind _ (PVar name') _ _ _) = name == name'
    matchBind name (FunBind xs) =
        and [name == name' | Match _ name' _ _ _ _ <- xs]
    matchBind _ _ = False

    -- match a bunch of declarations.
    matchDecl :: Maybe [Decl] -> Maybe [Decl] -> M ()
    matchDecl (Just ds1) (Just ds2) = go ds1 ds2
      where
        go (d1 : ds1) (d2 : ds2) = matchWithThis `mplus` matchWithNext -- extra declarations are allowed
          where
            matchWithThis = case d1 of
                  -- allow replacing  foo = undefined  by one or more
                  -- bindings of  foo.
                  PatBind _ (PVar name) _ (UnGuardedRhs u) (BDecls [])
                     | matchUndef (Just u) && matchBind name d2
                     -> go ds1 (dropWhile (matchBind name) ds2)
                  _ -> match d1 d2 >> go ds1 ds2
            matchWithNext =
                  go (d1 : ds1) ds2
        go [] [] = return ()
        go []  _ = return () -- extra declarations are allowed
        go _ _ = failLoc
    matchDecl _ _ = continue

    -- match syntax trees
    match :: (Data a, Data a1) => a -> a1 -> M a1
    match f1 f2 = do
        msum [
            -- 1. locations (see above)
            void $ matchLoc (cast f1) (cast f2),
            -- 2. declarations (see above)
            matchDecl (cast f1) (cast f2),
            -- 3. undefined  may be replaced by any expression
            when (not $ matchUndef (cast f1)) continue,
            -- otherwise, compare constructors and match arguments
            do  when (toConstr f1 /= toConstr f2) failLoc
                void (gzipWithM' match f1 f2)
            ]
        return f2

    noLoc = SrcLoc "<unknown.hs>" (-1) (-1)
  in
    evalState (runM (match m1' m2')) noLoc

------------------------------------------------------------------------------
-- | Twin map for monadic transformation
--
-- Unlike gzipWithM, process constructor arguments from left to right.
gzipWithM' :: Monad m => GenericQ (GenericM m) -> GenericQ (GenericM m)
gzipWithM' f x y = case gmapAccumM' perkid funs y of
                    ([], c) -> c
                    _       -> error "gzipWithM"
 where
  perkid a d = (tail a, unGM (head a) d)
  funs = gmapQ (\k -> GM (f k)) x

-- | gmapM with accumulation
--
-- Unlike gmapAccumM, process constructor arguments from left to right.
gmapAccumM' :: (Data d, Monad m)
           => (forall e. Data e => a -> e -> (a, m e))
           -> a -> d -> (a, m d)
gmapAccumM' f = gfoldlAccum k z
 where
  k a c d = let (a',d') = f a d
             in (a', c >>= \c' -> d' >>= \d'' -> return (c' d''))
  z a x = (a, return x)

------------------------------------------------------------------------------
-- tests

mod1 = unlines [
    "foo :: Int",
    "foo = undefined",
    "",
    "main :: IO ()",
    "main = print foo"
    ]

mod2 = unlines [
    "foo :: Int",
    "foo = 42",
    "",
    "main :: IO ()",
    "main = print foo"
    ]

mod3 = unlines [
    "foo :: Int",
    "foo | False = 23",
    "foo | True = 42",
    "",
    "main :: IO ()",
    "main = print fooo"
    ]

mod4 = unlines [
    "foo :: Double",
    "foo = undefined",
    "",
    "main :: IO ()",
    "main = print foo"
    ]
