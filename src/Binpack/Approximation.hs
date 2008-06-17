module Binpack.Approximation where

import Binpack.Instance

import Data.List ( sort )

first_fit :: Instance -> Assignment
first_fit i = 
    assign $ ff ( capacity i ) ( weights i )

first_fit_decreasing :: Instance -> Assignment
first_fit_decreasing i = 
    assign $ ff ( capacity i ) ( reverse $ sort $ weights i )

first_fit_decreasing_size :: Instance -> Int
first_fit_decreasing_size i = 
    length $ ff ( capacity i ) ( reverse $ sort $ weights i )

data Bin = Bin { cap :: Integer
               , contents :: [Integer]
               }
    deriving ( Show )

assign :: [ Bin ] -> Assignment
assign bins = map contents bins

ff :: Integer -- ^ capacity
   -> [ Integer ] -- ^ weights (finite)
   -> [ Bin ]
ff c ws = 
    let fun bins [] = bins
        fun bins (w : ws) = 
            let ( pre, midpost ) = span ( \ b -> cap b < w ) bins
                mid : post = case midpost of
                    [] -> Bin { cap = c, contents = [] } : []
                    _ -> midpost
                bins' = pre ++ Bin { cap = cap mid - w, contents = w : contents mid } : post
            in  fun bins' ws
    in  fun [] ws 

