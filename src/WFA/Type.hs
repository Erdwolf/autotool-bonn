module WFA.Type where

import Data.Map ( Map ) 
import qualified Data.Map as M

import Data.Set ( Set )
import qualified Data.Set as S

import WFA.Matrix ( Matrix )
import qualified WFA.Matrix

import WFA.Semiring ( Semiring )
import qualified WFA.Semiring

data WFA c s a = WFA { semiring :: Semiring a
                     , alphabet :: Set c
                     , states   :: Set s
                     , initial  :: Map s a
                     , transition :: Map c ( Matrix s a )
                     , final    :: Map s a
                     }

interpretation :: ( Ord c, Ord s ) => WFA c s a -> [c] -> Matrix s a
interpretation a w = 
    foldr ( WFA.Matrix.times )
          ( WFA.Matrix.unit ( semiring a ) ( states a ) ) $ do
        c <- w
        return $ M.findWithDefault ( error "interpretation" ) c $ transition a

weight :: ( Ord c, Ord s ) => WFA c s a -> [c] -> a
weight a w = foldr ( WFA.Semiring.plus $ semiring a ) ( WFA.Semiring.zero $ semiring a ) $ do
    ((x,y), h) <- M.toList $ WFA.Matrix.contents $ interpretation a w
    let get m x = M.findWithDefault ( WFA.Semiring.zero $ semiring a ) x m
    return $ foldr1 ( WFA.Semiring.times $ semiring a ) 
           [ get ( initial a ) x , h, get ( final a ) y ]


