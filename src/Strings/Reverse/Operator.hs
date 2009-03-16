-- TODO: this is a generice module with sequence operations

module Strings.Reverse.Operator where

import Autolib.Util.Zufall
import Control.Monad ( forM )

slice xs = do
    m <- randomRIO ( 1, length xs - 1 )
    s <- randomRIO ( 0, length xs - m )
    let ( pre, midpost ) = splitAt s xs
        ( mid, post ) = splitAt m midpost
    return ( pre, mid, post )

-- mutations

cut xs = do
    ( pre, mid, post ) <- slice xs
    return $ pre ++ post
                          
shift xs = do
    ( pre, mid, post ) <- slice xs
    off <- eins [ -1, 1 ]
    return $ pre ++ map (off +) mid ++ post

uniform xs = forM xs $ \ x -> do
    act <- randomRIO ( 0, length xs )
    if act == 0
       then eins [ x-1, x+1 ] 
       else return x
    
-- combinations
onepoint xs ys = do
    i <- randomRIO ( 1, length xs ) 
    j <- randomRIO ( 0, length ys - 1 )
    return $ take i xs ++ drop j ys


twopoint xs ys = do
    ( pre, _, post ) <- slice xs
    ( _, mid, _ ) <- slice ys
    return $ pre ++ mid ++ post

zipper [] ys = return ys
zipper xs [] = return xs
zipper (x:xs) ys = do
    flip <- randomRIO ( False, True )
    let here = [ x | flip ]
    rest <- zipper ys xs
    return $ here ++ rest
