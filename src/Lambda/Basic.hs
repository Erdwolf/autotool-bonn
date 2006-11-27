module Lambda.Basic where

import Lambda.Data

i :: Lambda
i = read "x -> x"

s :: Lambda
s = read "x y z -> x z (y z)"

k :: Lambda
k = read "x y -> x"

