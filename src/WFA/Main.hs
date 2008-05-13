import WFA.Matrix

import WFA.Type

import WFA.RGB

import Data.Map ( Map ) 
import qualified Data.Map as M

import Data.Set ( Set )
import qualified Data.Set as S

import Data.Array

main :: IO ()
main = writePBM "bild.pbm" a 3
       

data LR = L | R deriving ( Eq, Ord, Show, Enum, Bounded )
data UD = U | D deriving ( Eq, Ord, Show, Enum, Bounded )

type Quad = ( LR, UD )

quads :: [ Quad ]
quads = do 
    h <- [ minBound .. maxBound ]
    v <- [ minBound .. maxBound ]
    return ( h, v )

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

writePBM file aut dep = do
    let pic = picture aut dep
        w = 2^dep
        contents = unlines $ do
            row <- [ 0 .. w - 1 ]
            return $ unwords $ do
                col <- [ 0 .. w - 1 ]
                return $ show $ pic ! (row, col)
    writeFile file $ unlines $
        [ "P3"
        , unwords [ show w, show w ]
        , show 255
        , contents
        ]


type Path = [ Quad ]

paths :: Int -> [ Path ]
paths 0 = return []
paths d | d > 0 = do
    x <- quads
    rest <- paths ( d - 1 )
    return $ x : rest

type Point = ( Int, Int )

position :: Path -> Point
position p =
    let f (x,y) [] = (x, y)
        f (x,y) ((h,v) : rest) =
            let hh = fromEnum h
                vv = fromEnum v
            in  f ( 2 * x + hh, 2 * y + vv ) rest
    in  f (0,0) p

picture aut dep =
    let top = 2^dep - 1
        bnd = ((0,0),(top,top))
    in  array bnd $ do
            p <- paths dep
            return ( position p, weight aut p )

