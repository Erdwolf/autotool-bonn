{-# LANGUAGE NoMonomorphismRestriction, TupleSections, DeriveDataTypeable #-}
module Syntax.Transformer where

import Syntax.Syntax

import Data.List
import Data.Maybe
import Data.Graph (stronglyConnComp, flattenSCCs)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Generics
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Identity
import Control.Arrow
import Control.Applicative



pop = do
    (x:xs) <- get
    put xs
    return x

removeLoops lang =
    fst $ flip runState fresh $ do
        (xs,ys) <- runWriterT (everywhereM (mkM removeLoop) lang)
        return (xs++ys)
  where
    fresh = [ "Loop_" ++ show i | i <- [1..] ] -- Assuming no existing non-terminal contains digits.

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
  where
    paths (Fork g1 g2)  = paths g1 ++ paths g2
    paths (Chain g1 g2) = [ Chain x y | x <- paths g1, y <- paths g2 ]
    paths x             = [x]


data Grammar = Grammar
    { startSymbol :: String
    , containsEmptyWord :: Maybe Bool
    , rules :: [Rule]
    }
grammar s0 eps rs = Grammar s0 eps (canonicalize rs)

fromRules rs@(Rule s0 _:_) = grammar s0 Nothing rs
fromRules []               = grammar (error "No start symbol") Nothing []

data Rule = Rule { lhs :: String, rhs :: [Item]} deriving (Show, Eq, Data, Typeable)
data Item = T String | N String deriving (Show, Eq, Data, Typeable)


printGrammar = putStrLn . showGrammar

showGrammar (Grammar s0 eps rs) = intercalate "\n" $ ["["++s0++"]" ++ e] ++  map (\(Rule l rs) -> l ++ " -> " ++ intercalate " " (map f rs)) rs
    where f (N n) = n; f (T t) = t
          e = case eps of Just True ->  " ε"; Just false -> ""; ~othing -> " ?"

toGrammar :: Language -> Grammar
toGrammar l = fromRules $ map toRule l

fromGrammar :: Grammar -> Language
fromGrammar (Grammar s0 eps rs) = map fromRule $ sortBy (comparing ((/=s0).lhs)) $ Rule s0 [] : rs

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
chomsky :: Grammar -> Grammar
--chomsky (Grammar s0 Nothing []) = Grammar s0 (Just False) []
--chomsky (Grammar s0 eps []) = Grammar s0 eps []
chomsky g =
        binarize $
        terminalsAsLeafs $
        eliminateChainRules $
        eliminateCycles $
        eliminateEmptyRules $
        g

canonicalize :: [Rule] -> [Rule]
canonicalize = sortBy (comparing lhs) . nub

addRules :: [Rule] -> Grammar -> Grammar
addRules rs1 (Grammar s0 eps rs2) = Grammar s0 eps (canonicalize (rs1 ++ rs2))

combinations :: Eq a => (a -> [[a]]) -> [a] -> [[a]]
combinations = fmap concat . mapM

partitionEmpty :: [Rule] -> ([String],[String])
partitionEmpty rs =
    let v1 = go $ filter (\a -> Rule a [] `elem` rs) nonterminals
    in (v1, nonterminals \\ v1)

   where
      go v1 = -- trace (show v1) $
        let new = [ b | Rule b xs <- rs, b `notElem` v1, as <- onlyNonterms xs, as `subset` v1 ] in
        if null new
           then v1
           else go $ v1 `union` new

      nonterminals = nub $ map lhs rs

onlyNonterms :: (Functor m, MonadPlus m) => [Item] -> m [String]
onlyNonterms (N n:xs) = (n:) <$> onlyNonterms xs
onlyNonterms []       = return []
onlyNonterms _        = mzero

subset x s = nub x \\ s == []


eliminateEmptyRules :: Grammar -> Grammar
eliminateEmptyRules (Grammar s0 eps originalRules) =
    let (v1, v2) = partitionEmpty originalRules           -- Which symbols can derive the empty word?
        eps' = Just (fromMaybe False eps || s0 `elem` v1) -- Can the start symbol derive the empty word?
        phase1 = filter (\(Rule a xs) -> not (null xs))   -- Remove all empty rules
        phase2 =                                          -- For each B -> xAy, A in V¹, x and y not both empty; add B -> xy
            (++) $ do
                Rule b xs <- originalRules
                let c (N a) | a `elem` v1 = [[N a],[]]
                    c x                   = [[x]]
                xss' <- mapM c xs
                let xs' = concat xss'
                guard (not (null xs'))
                return (Rule b xs')
    in grammar s0 eps' $ phase2 $ phase1 $ originalRules


eliminateCycles :: Grammar -> Grammar
eliminateCycles (Grammar s0 eps@(Just _) originalRules) =
    grammar (f s0) eps $ dropDirectCycles $ mergeCycles originalRules

  where
    dropDirectCycles =
        filter $ \(Rule a xs) -> case xs of
                                   [N b] | a == b -> False -- This is a direct cycle
                                   _              -> True

    mergeCycles =
        map $ \(Rule a xs) -> Rule (f a) (map g xs)

    g (N a) = N (f a)
    g x     = x
    f a = case lookup a table of
            Just b  -> b
            Nothing -> a

    table = [ (x,newName xs) | xs <- nontermsThatHaveToBeMerged, x <- xs ]

    newName = intercalate "+"

    nontermsThatHaveToBeMerged = equivClasses $ findCycles [ (a,b) | Rule a [N b] <- originalRules ]

equivClasses :: Eq a => [[a]] -> [[a]]
equivClasses []       = []
equivClasses (xs:xss) =
    let (e,ne) = partition (xs `intersects`) xss
    in if null e
          then xs : equivClasses xss
          else equivClasses $ nub (concat (xs : e)) : ne

intersects :: Eq a => [a] -> [a] -> Bool
intersects a b = intersect a b /= []


findCycles :: Eq a => [(a,a)] -> [[a]]
findCycles graph = cycles Nothing allVertices []
   where
    cycles Nothing  not_visited@(v:_) []     = cycles (Just v) not_visited []
    cycles Nothing  []                []     = []
    cycles (Just v) not_visited       prefix = -- trace ("\nVertex: " ++ show v ++ "\nPrefix = " ++ show prefix ++ "\n") $
        let
            nexts            = [ n | (t,n) <- graph, t == v, n `elem` not_visited ]
            prefix'          = prefix ++ [v]
            verticesInCycles = [ n | (t,n) <- graph, t == v, n `elem` prefix' ]
            not_visited'     = not_visited \\ [v]
        in nub $ map (testcycle prefix') verticesInCycles ++ if null nexts then cycles Nothing not_visited' []
                                                                           else concat [ cycles (Just n) not_visited' prefix' | n <- nexts ]
      where
        testcycle (x:xs) n | x == n = nub $ x:xs
        testcycle (_:xs) n          = testcycle xs n
        testcycle []     n          = []

    allVertices = nub $ graph >>= \(a,b) -> [a,b]


eliminateChainRules :: Grammar -> Grammar
eliminateChainRules (Grammar s0 eps originalRules) = -- trace (show nonterminals_topSorted) $
    grammar s0 eps $ foldr g originalRules nonterminals_topSorted
  where
    g a allRules = f allRules
        where
          f (Rule a' [N b]:rs) | a == a', b `gt` a =
              -- Eliminate this rule
              [ Rule a xs | Rule b' xs <- allRules, b == b' ] ++ f rs
          f (r:rs) =
              -- Keep this rule
              r : f rs
          f [] =
              []

    nonterminals_topSorted =
      reverse $
       flattenSCCs $
         stronglyConnComp $
           [ (a,a,nub [ b | Rule a' [N b] <- originalRules, a == a' ]) | a <- nonterminals  ]

    a `gt` b = -- trace (show a) $
        let Just i = findIndex (==a) nonterminals_topSorted
            Just j = findIndex (==b) nonterminals_topSorted
        in i > j

    nonterminals = nub $ do Rule a [N b] <- originalRules; [a,b]


terminalsAsLeafs :: Grammar -> Grammar
terminalsAsLeafs (Grammar s0 eps originalRules) = grammar s0 eps $ introduceTerminalRules $ toNontermRules originalRules
  where
      toNontermRules rs = [ if isLeaf xs then Rule a xs else Rule a (map toNonterm xs) | Rule a xs <- rs ]
      toNonterm (T t) = N (t ++ "°")
      toNonterm x     = x
      isLeaf [T _] = True
      isLeaf [N _] = error "Input contained chain rule"
      isLeaf _     = False
      introduceTerminalRules = ([ Rule (t ++ "°") [T t] | t <- terminals ] ++)
      terminals = concat [ [ t | T t <- rhs r ] | r <- originalRules ]


binarize :: Grammar -> Grammar
binarize (Grammar s0 eps originalRules) = grammar s0 eps (f originalRules)
  where
      f (Rule a (b:(twoOrMore@(_:_:_))):rs) = Rule a [b,N new] : f (Rule new twoOrMore : rs)  where new = a ++ "~"
      f (r:rs) = r : f rs
      f [] = []
