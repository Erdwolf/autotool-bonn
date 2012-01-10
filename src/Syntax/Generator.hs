{-# LANGUAGE NoMonomorphismRestriction, TupleSections #-}
module Syntax.Generator where

import Syntax.Syntax
import Syntax.Generics
import Syntax.Words

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import System.Random
import Control.Monad.State

import Data.List

--generate :: IO [(String,Graph)]
--generate = generateWithConfig defaultConfig

data GeneratorConfig = GC
    { gc_terminals    :: [String]
    , gc_nonterminals :: [String]
    , gc_sizes        :: [Int]
    , gc_requiredWords      :: Int
    , gc_maxSteps           :: Int
    , gc_minDiagramsUsed    :: Int
    , gc_minStepsForLongest :: Int
    , gc_frequencies :: Frequencies
    }

defaultConfig :: GeneratorConfig
defaultConfig = GC
    { gc_terminals    = ["a","b","c","d"]
    , gc_nonterminals = ["A","B","C","D"]
    , gc_sizes        = [6,4,4,4]
    , gc_requiredWords      = 4
    , gc_maxSteps           = 7
    , gc_minDiagramsUsed    = 3
    , gc_minStepsForLongest = 5
    , gc_frequencies = defaultFrequencies
    }

data Frequencies = FQ
    { fq_chain :: Int
    , fq_fork  :: Int
    , fq_opt   :: Int
    , fq_loop  :: Int
    , fq_term  :: Int
    , fq_nterm :: Int
    }
defaultFrequencies = FQ 50 10 10 5 15 15


generateWithConfig :: GeneratorConfig -> IO [(String,Graph)]
generateWithConfig cfg = loop
 where
   loop = do
      l <- getRandomLang
      --putStrLn "===================="
      --putStrLn $ intercalate "\n" $ concatMap (ascii . snd) l
      if isNice l
         then return l
         else loop

   isNice l =
      gc_terminals cfg `subset` terminals l
      &&
      let smallestWords = take (gc_requiredWords cfg) $ generateWords (gc_maxSteps cfg) l in
      gc_requiredWords cfg == length smallestWords
      &&
      let nonterminalsInvolved = nub $ filter (`elem` gc_nonterminals cfg) $ concat $ map snd smallestWords in
      gc_minDiagramsUsed cfg == length (take (gc_minDiagramsUsed cfg) nonterminalsInvolved)
      &&
      any ((gc_minStepsForLongest cfg <=).length.filter (`elem` gc_nonterminals cfg).snd) smallestWords


   subset xs ys = all (`elem` ys) xs

   getRandomLang = getStdRandom $
      maybe (error "Error while generating language.") id . runStateT lang

   lang = sequence [ (a,) <$> evalStateT graph k | (a,k) <- zip (gc_nonterminals cfg) (gc_sizes cfg) ]

   mustContain xs g = do
       guard (all (`elem`symbols g) xs)
       return g

   graph = do
      n <- get
      guard (n > 0)
      frequency
            [ (fq_chain fq, Chain <$> graph' <*> graph')
            , (fq_fork  fq, Fork  <$> graph' <*> graph')
            , (fq_opt   fq, Fork  <$> graph' <*> return Empty)
            , (fq_loop  fq, Loop  <$> graph')
            , (fq_term  fq, Terminal <$> terminal)
            , (fq_nterm fq, Symbol   <$> symbol)
            ]
         where graph' = (modify pred) >> graph
               fq = gc_frequencies cfg

   terminal = oneof $ map return (gc_terminals cfg)
   symbol = oneof $ map return (gc_nonterminals cfg)

oneof [] = fail "oneof used with empty list"
oneof gs = do
   i <- choose (0,length gs - 1)
   (gs !! i) <|> (oneof (gs \! i))

{-|
  Chooses one of the given generators, with a weighted random distribution.
  The input list must be non-empty.
-}
frequency [] = fail "frequency used with empty list"
frequency xs0 = choose (1 :: Int, tot) >>= (\n -> pick n xs0 0)
 where
  tot = sum (map fst xs0)

  pick n ((k,x):xs) i
    | n <= k    = x <|> frequency (xs0 \! i)
    | otherwise = pick (n-k) xs (succ n)
  pick _ _  _ = fail "pick used with empty list"

(\!) :: [a] -> Int -> [a]
xs \! i = take i xs ++ drop (succ i) xs


choose rng = do
   g <- lift get
   let (x,g') = randomR rng g
   lift (put g')
   return x
