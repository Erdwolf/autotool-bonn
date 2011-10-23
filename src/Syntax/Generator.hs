{-# LANGUAGE NoMonomorphismRestriction, TupleSections, DeriveDataTypeable, StandaloneDeriving  #-}
module Syntax.Generator where

import Syntax.Syntax

import Control.Applicative
import Control.Monad
import Control.Monad.Reader
import System.Random
import Control.Monad.State

import Data.Generics

deriving instance Typeable Graph
deriving instance Data Graph

generate :: IO [(String,Graph)]
generate = do
   l <- getRandomLang
   if isNice l
      then return l
      else generate

isNice l =
   all (`elem` terminals l) ["a","b","c","d"]

getRandomLang = getStdRandom $
   maybe (error "Error while generating language.") id . runStateT lang

--instance MonadPlus (RandT StdGen Maybe) where
--   m1 `mplus` m2 = runRandT

-- >>= \g -> guard () >> return g

terminals = everything (++) ([] `mkQ` q)
 where
   q (Terminal t) = [t]
   q _            = []

symbols = everything (++) ([] `mkQ` q)
 where
   q (Symbol s) = [s]
   q _          = []


lang = sequence
         [ ("A",) <$> runReaderT graph 4
         , ("B",) <$> runReaderT graph 4
         , ("C",) <$> runReaderT graph 4
         , ("D",) <$> runReaderT graph 4
         ]

graph = do
   n <- ask
   guard (n > 0)
   oneof
         [ Chain <$> graph' <*> graph'
         , Fork  <$> graph' <*> graph'
         , Loop  <$> graph'
         , Terminal <$> terminal
         , Symbol   <$> symbol
         , return Empty
         ]
      where graph' = local pred graph

terminal = oneof $ map return ["a","b","c","d"]
symbol = oneof $ map return ["A","B","C","D"]

oneof [] = fail "oneof used with empty list"
oneof gs = do
   i <- choose (0,length gs - 1)
   (gs !! i) <|> (oneof (gs \! i))

(\!) :: [a] -> Int -> [a]
xs \! i = take i xs ++ drop (succ i) xs


choose rng = do
   g <- get
   let (x,g') = randomR rng g
   put g'
   return x
