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



generate :: IO [(String,Graph)]
generate = do
   l <- getRandomLang
   if isNice l
      then return l
      else generate

isNice l =
   ["a","b","c","d"] `subset` terminals l
   &&
   4 == length (take 4 $ generateWords 15 l)

subset xs ys = all (`elem` ys) xs

getRandomLang = getStdRandom $
   maybe (error "Error while generating language.") id . runStateT lang

--instance MonadPlus (RandT StdGen Maybe) where
--   m1 `mplus` m2 = runRandT

-- >>= \g -> guard () >> return g


lang = sequence
         [ ("A",) <$> evalStateT graph 6
         , ("B",) <$> evalStateT graph 4
         , ("C",) <$> evalStateT graph 4
         , ("D",) <$> evalStateT graph 4
         ]

graph = do
   n <- get
   guard (n > 0)
   oneof
         [ Chain <$> graph' <*> graph'
         , Fork  <$> graph' <*> graph'
         , Loop  <$> graph'
         , Terminal <$> terminal
         , Symbol   <$> symbol
         , return Empty
         ]
      where graph' = (modify pred) >> graph

terminal = oneof $ map return ["a","b","c","d"]
symbol = oneof $ map return ["A","B","C","D"]

oneof [] = fail "oneof used with empty list"
oneof gs = do
   i <- choose (0,length gs - 1)
   (gs !! i) <|> (oneof (gs \! i))

(\!) :: [a] -> Int -> [a]
xs \! i = take i xs ++ drop (succ i) xs


choose rng = do
   g <- lift get
   let (x,g') = randomR rng g
   lift (put g')
   return x
