module Syntax.Printer where

import Syntax.Syntax
import Data.Ratio

type Block = [String]

height, width :: Block -> Int
height = length
width []    = 0
width (x:_) = length x

ascii :: Graph -> [String]
ascii g =
  case g of
    Chain    g1 g2 -> ascii g1 `h` ascii g2
    Fork     g1 g2 ->
        let b1 = ascii g1
            b2 = ascii g2
        in fork (height b1) `h` (b1 `v` [" "] `v` b2) `h` join (height b1)
    Loop     g     ->
        let b = ascii g
        in loop (height b) `h` (b `v` [" "] `v` ["-"]) `h` unloop (height b)
    Terminal t     -> ["-("] `h` [t] `h` [")-"]
    Symbol   s     -> ["-["] `h` [s] `h` ["]-"]
    Empty          -> ["-"]

fork 1 = ["--"]  `v`
         [" \\"] `v`
         ["  "]
fork n = ["---"] `v`
         [" \\ "] `v`
         replicate (n-2) " | " `v`
         [" \\ "] `v`
         ["  -"]

join 1 = ["--"] `v`
         ["/ "] `v`
         ["  "]
join n = ["---"] `v`
         [" / "] `v`
         replicate (n-2) " | " `v`
         [" / "] `v`
         ["-  "]


loop 1 = ["---"] `v`
         [" / "] `v`
         [" --"]
loop n = ["---"] `v`
         [" / "] `v`
         replicate (n-2) " | " `v`
         [" / "] `v`
         [" --"]

unloop 1 = ["---"]  `v`
           [" \\ "] `v`
           ["-- "]
unloop n = ["---"] `v`
           [" \\ "] `v`
           replicate (n-2) " | " `v`
           [" \\ "] `v`
           ["---"]

h, v :: Block -> Block -> Block

h b1 b2 =
  let m = max (height b1) (height b2) in
  equalizeWidth (zipWith (++) (fill m " " b1) (fill m " " b2))

v b1 b2 = equalizeWidth (equalizeWidth b1 ++ equalizeWidth b2)


equalizeWidth :: Block -> Block
equalizeWidth xss =
  let m = maximum (map length xss) in
  map (center {- or `extend` for left align -} m) xss

center :: Int -> [Char] -> [Char]
center newSize xs =
  let oldSize = length xs in
  let n = newSize - oldSize in
  let l = head xs in
  let r = last xs in
  replicate (floor (n%2)) (more l) ++ xs ++ replicate (ceiling (n%2)) (more r)

extend :: Int -> [Char] -> [Char]
extend size xs =
  let c = last xs in
  fill size (more c) xs

more c | c `elem` "\\|/" = ' '
       | otherwise       =  c -- '"'

fill :: Int -> a -> [a] -> [a]
fill n x xs = xs ++ replicate (n - length xs) x


example = Fork (Terminal "a" `Chain` Terminal "b")
               (Symbol "S")

example2 = Fork (Symbol "S")
                Empty

example3 = Fork (Terminal "a" `Chain` Terminal "b")
                (Fork (Symbol "S")
                      Empty)

example4 = Fork (Fork (Terminal "a" `Chain` Terminal "b")
                      (Symbol "S"))
                Empty

example5 = Fork (Loop (Terminal "a"))
                (Loop (Terminal "b"))
