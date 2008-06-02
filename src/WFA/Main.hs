import WFA.Matrix
import WFA.Type
import WFA.RGB
import WFA.Write

import Data.Map ( Map ) 
import qualified Data.Map as M

import Data.Set ( Set )
import qualified Data.Set as S


main :: IO ()
main = writePBM "bild-b.pbm" Main.b 9
       

a :: WFA Quad Int RGB 
a = let s = WFA.RGB.maxplus
    in  WFA 
        { WFA.Type.semiring = s
        , alphabet = S.fromList quads
        , states   = S.fromList [ 1,2 ]
        , initial  = WFA.Matrix.make s [ ( (), blue, 1 ) ]
        , transition = M.fromList
              [ ( (L,U), WFA.Matrix.make s [ (1, white, 2) ] )
              , ( (L,D), WFA.Matrix.make s [ (1, white, 2) ] )
              , ( (R,U), WFA.Matrix.make s [ (1, white, 2) ] )
              , ( (R,D), WFA.Matrix.make s [ (1, white, 2) ] )
              ]
        , final    = WFA.Matrix.make s [ ( 2, red, () ) ]
        }

b :: WFA Quad Int RGB 
b = let s = WFA.RGB.maxplus2
    in  WFA 
        { WFA.Type.semiring = s
        , alphabet = S.fromList quads
        , states   = S.fromList [ 1,2 ]
        , initial  = WFA.Matrix.make s [ ( (), white, 1 ), ( (), white, 2 ) ]
        , transition = M.fromList
              [ ( (L,U), WFA.Matrix.make s [ (1, red, 2), (1, blue, 1) ] )
              , ( (L,D), WFA.Matrix.make s [ (2, green, 1), (2, blue, 2) ] )
              , ( (R,U), WFA.Matrix.make s [ (1, white, 1), (1, red, 2) ] )
              , ( (R,D), WFA.Matrix.make s [ (1, red, 1), (1, green, 2) ] )
              ]
        , final    = WFA.Matrix.make s [ ( 1, white, () ), ( 2, white, () ) ]
        }






