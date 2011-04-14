{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Graph.MST.Quiz where

import Graph.MST.Plain ( MST ( MST ) )

import Graph.MST.Config 

import Autolib.Util.Zufall ( selektion, randomRIO )
import Graph.Weighted

import Inter.Quiz ( Generator , generator , Project , project , quiz )
import Inter.Types ( Make )

import Control.Monad ( forM )
import Data.List ( tails )
import qualified Data.Set as S

instance Generator MST Config ( Graph Int Int ) where
    generator MST conf key = do
       let v = [ 1 .. nodes conf ]
       xys <- selektion ( edges conf )
          $ do (x : ys) <- tails v ; y <- ys ; return (x, y)
       e <- forM xys $ \ (x,y) -> do
           w <- randomRIO $ weight_bounds conf
           return $ Kante { von = x, nach = y, gewicht = w }
       return $ Graph { knoten = S.fromList v, kanten = S.fromList e }

instance Project MST ( Graph v w ) ( Graph v w )  where 
    project MST g = g

make :: Make
make = quiz MST rc
