{-# LANGUAGE NoMonomorphismRestriction, TupleSections, DeriveDataTypeable #-}
module Syntax.Transformer where

import Syntax.Syntax
import Syntax.Generics

import Data.List
import Data.Generics
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity
import Control.Arrow

transform :: Language -> Language
transform = onGrammar (removeLeftRecursion . removeEmpties)
          . removeForks
          . removeLoops



onGrammar trans = fromGrammar . trans . toGrammar

removeLoops lang =
    let fresh = [ "Loop_" ++ show i | i <- [1..] ] in -- Assuming no existing non-terminal contains digits.
    fst $ flip runState fresh $ do
        (xs,ys) <- runWriterT (everywhereM (mkM removeLoop) lang)
        return (xs++ys)

removeLoop (Loop g) = do
    x <- pop
    tell [ (x, g)
         , (x, g `Chain` Symbol x)
         ]
    return (Symbol x)
removeLoop g = return g

removeForks [] = []
removeForks ((x, g):rest) =
    let new = map (x,) $ paths g in
    new ++ removeForks rest

reorderEmpties rs =
    let (empty, nonempty) = partitionEmpties rs in
    nonempty ++ empty

removeEmpties rs@(Rule s _:_) =
    let (empty, nonempty) = partitionEmpties rs
        emptyRule | Rule s [] `elem` empty = [Rule "0" [N s], Rule "0" []]
                  | otherwise              = []          in
    emptyRule ++ nonempty ++ [ Rule y (delete (N x) ys) | Rule x _ <- empty, Rule y ys <- nonempty, N x `elem` ys ]
  where

partitionEmpties = partition (null.rhs)

paths (Fork g1 g2)  = paths g1 ++ paths g2
paths (Chain g1 g2) = [ Chain x y | x <- paths g1, y <- paths g2 ]
paths x             = [x]

pop = do
    (x:xs) <- get
    put xs
    return x

data Rule = Rule { lhs :: String, rhs :: [Item]} deriving (Show, Eq, Data, Typeable)
data Item = T String | N String deriving (Show, Eq, Data, Typeable)

toGrammar :: Language -> [Rule]
toGrammar = map toRule

fromGrammar :: [Rule] -> Language
fromGrammar = map fromRule

toRule :: (String, Graph) -> Rule
toRule (x,g) = Rule x $ map toItem $ filter (/=Empty) $ everything (++) ([] `mkQ` q) g
  where q (Chain _ _) = []
        q g           = [g]

toItem :: Graph -> Item
toItem (Symbol   x) = N x
toItem (Terminal x) = T x


fromRule :: Rule -> (String, Graph)
fromRule (Rule x g) = (x, foldr Chain Empty $ map fromItem g)

fromItem :: Item -> Graph
fromItem (N x) = Symbol x
fromItem (T x) = Terminal x


-- Transform to Chomsky-Normal-Form
chomsky :: [Rule] -> [Rule]
chomsky [] = []
chomsky rs = flip evalState [ "cnf"++show i | i <- [1..] ] $ do  -- Assuming no existing non-terminal contains digits.
    let s = lhs (head rs)
    s0 <- pop
    let
      addingNewRules = liftM (nub . uncurry (++)) . runWriterT

      introduceNewStartSymbol rs = return (Rule s0 [N s] : rs)

      -- FIXME Eliminate empty rules in the newly generated ones, until nothing is left.
      eliminateEmptyRules allRules = flip evalStateT [] $ do
        let
          --e :: Rule -> StateT [String] (WriterT [Rule] (State [String])) Bool
          e (Rule a []:rs) | a /= s = do
             -- Eliminate
             let
               f (Rule b [N a']) | a == a' = do
                  alreadyRemoved <- gets (b `elem`)
                  if alreadyRemoved
                     then return []
                     else return [Rule b []]
               f (Rule b rhs)| N a `elem` rhs  = do
                 return $ Rule b `liftM` combinations (N a) rhs
               f _ = return []
             newRules <- mapM f allRules
             modify (a:)
             e (concat (rs : newRules))
          e (r:rs) = do
             -- Keep
             (r:) `liftM` e rs
          e [] = return []
        e rs

      eliminateUnitRules rs = addingNewRules $ flip evalStateT [] $ do
        let
          e :: Rule -> StateT [Rule] (WriterT [Rule] (State [String])) Bool
          e r@(Rule a [N b]) = do
             let
               f (Rule b' u) | b == b' = do
                  alreadyRemoved <- gets (Rule a u `elem`)
                  unless alreadyRemoved $ do
                    tell [Rule a u]
               f r = tell [r]
             mapM_ f rs
             modify (r:)
             return False
          e _ = return True
        filterM e rs

      removeUselessProductions rs = do
        let
          e (Rule _ [N _]) = False
          e _              = True
        return (filter e rs)

      cleanUpLongRules rs = addingNewRules $ do
        let
          f :: Item -> Item -> WriterT [Rule] (State [String]) Item
          f b1 b2 = do
              b1' <- n b1
              b2' <- n b2
              x <- pop
              tell [Rule x [b1',b2']]
              return (N x)

          n (T x) = do
              tell [Rule x [T x]]
              return (N x)
          n b = return b

          g :: Rule -> WriterT [Rule] (State [String]) Rule
          g (Rule a (b:bs@(_:_))) = do
            c <- foldM1 f bs
            b' <- n b
            c' <- n c
            return (Rule a [b',c'])
          g r = return r
        mapM g rs

    return rs
     >>= introduceNewStartSymbol -- Introduce new start variable
     >>= eliminateEmptyRules
     >>= eliminateUnitRules
     >>= removeUselessProductions
     >>= cleanUpLongRules


combinations :: Eq a => a -> [a] -> [[a]]
combinations x (y:ys) | x == y = do
    zs <- combinations x ys
    [zs, y:zs]
combinations x (y:ys) = do
    (y:) `liftM` combinations x ys
combinations _ [] = do
    [[]]

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 f (x:xs) = foldM f x xs

-- Paull's algorithm
--
removeLeftRecursion lang = fst $ flip runState [ "lr"++show i | i <- [1..] ] $ do  -- Assuming no existing non-terminal contains digits.
    f (zip [0..] xs) lang
  where
    xs = map lhs lang -- Arbitrarily ordered list of all non-terminals

    f :: [(Int,String)] -> [Rule] -> State [String] [Rule]
    f [] rs = return rs
    f ((i,x):ixs) rs = f ixs =<< removeDirect =<< return (g (take i xs) rs)
      where
        g [] l = l
        g (y:ys) l = g ys $ h l
          where
            h [] = []
            h ((Rule x' (N y':a)):rs) | x == x' && y == y' =
                [ Rule x (b++a) | Rule y' b <- l, y == y' ] ++ h rs
            h (r:rs) = r : h rs

        removeDirect :: [Rule] -> State [String] [Rule]
        removeDirect rs = liftM concat $ sequence $ map h rs
          where
            h :: Rule -> State [String] [Rule]
            h (Rule x' (N x'':a)) | x == x' && x' == x'' = do
                new <- pop
                return $ concat [ [Rule x (b++[N new]), Rule new a, Rule new (a++[N new])] | Rule x' b <- rs, x == x', take 1 b /= [N x] ]
            h r = return [r]
