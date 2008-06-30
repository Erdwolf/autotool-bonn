module Pentomino.Force where

import Pentomino.Position
import Pentomino.Cover hiding ( x, y )

import Autolib.Util.Splits
import Autolib.Util.Zufall

import Data.Set ( Set )
import qualified Data.Set as S

import Data.Complex

-- | center is at 0
away_from_center :: Set Position -> Complex Double
away_from_center ps = sum $ do
    p <- S.toList ps
    let c = tocom p
    return $ c / (1 + abs c)**2.0 

towards_each_other :: Set Position -> [ Set Position ] -> Complex Double
towards_each_other ps qss = sum $ do
    qs <- qss
    p <- S.toList ps
    q <- S.toList qs
    let d = tocom q - tocom p
    return $ d / (1 + abs d)**0.2

totals :: [ Set Position ] -> [ Complex Double ]
totals qss = do
    ( pre, qs : post ) <- splits qss
    let others = pre ++ post
    return $  2 * away_from_center qs
	   +  1 * towards_each_other qs others

total :: Figure -> Double
total f = 
    let fs = totals $ covers f
    in  maximum $ map ( realPart . abs ) fs

tocom :: Position -> Complex Double
tocom p = fromIntegral (x p) :+ fromIntegral (y p)

step :: Figure -> IO Figure
step f = do
    let offs = totals $ covers f
	most = maximum $ map ( realPart . abs ) offs
    ps <- sequence $ do
        ( p, off ) <- zip ( pieces f ) 
		    $ map ( \ o -> 5 * o / ( most :+ 0 ) ) offs 
        return $ do
            -- print ( off )
            iff <- fromcom off
            return $ p { shift = shift p + iff }
    return $ figure_shift ps

fromcom :: Complex Double -> IO Position
fromcom c = do
    let r = floor $ realPart c
    rr <- eins [ r-1, r, r + 1 ]
    let i = floor $ imagPart c
    ii <- eins [ i-1, i, i + 1 ]
    return $ Position rr ii
