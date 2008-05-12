module WFA.RGB where

import WFA.Semiring 

data RGB = RGB { r :: Int, g :: Int, b :: Int }

instance Show RGB where 
    show x = unwords $ map show [ r x, g x, b x ]


black :: RGB
black = RGB 0 0 0

white :: RGB
white = RGB top top top

red :: RGB
red = RGB top 0 0 

green :: RGB
green = RGB 0 top 0 

blue :: RGB
blue = RGB 0 0 top


maxplus :: Semiring RGB
maxplus = Semiring
        { zero = black
        , one  = white
        , plus = map2 max
        , times = map2 ( \ x y -> min top ( x + y ) )
        }

top :: Int
top = 255

map2 :: ( Int -> Int -> Int ) -> RGB -> RGB -> RGB
map2 f x y =  RGB { r = f ( r x ) ( r y )
                  , g = f ( g x ) ( g y )
                  , b = f ( b x ) ( b y )
                  }



    