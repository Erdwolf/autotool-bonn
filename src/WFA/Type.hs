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
                     , initial  :: Matrix () s a
                     , transition :: Map c ( Matrix s s a )
                     , final    :: Matrix s () a
                     }

interpretation :: ( Ord c, Ord s ) => WFA c s a -> [c] -> Matrix s s a
interpretation a w = 
    foldr ( WFA.Matrix.times )
          ( WFA.Matrix.unit ( semiring a ) ( states a ) ) $ do
        c <- w
        return $ M.findWithDefault ( error "interpretation" ) c $ transition a

final_interpretation :: ( Ord c, Ord s ) => WFA c s a -> [c] -> Matrix s () a
final_interpretation a w = 
    foldr ( WFA.Matrix.times ) ( final a ) $ do
        c <- w
        return $ M.findWithDefault ( error "interpretation" ) c $ transition a

weight :: ( Ord c, Ord s ) => WFA c s a -> [c] -> a
weight a w =  
    let res = WFA.Matrix.times ( initial a ) 
            $ final_interpretation a w 
    in  WFA.Matrix.get res ( (),() )



