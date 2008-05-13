import WFA.Matrix

import WFA.Type

import WFA.RGB

import Data.Map ( Map ) 
import qualified Data.Map as M

import Data.Set ( Set )
import qualified Data.Set as S

import Data.Array

main :: IO ()
main = writePBM "bild-b.pbm" Main.b 8
       

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

b :: WFA Quad Int RGB 
b = let s = WFA.RGB.maxplus2
    in  WFA 
        { WFA.Type.semiring = s
        , alphabet = S.fromList quads
        , states   = S.fromList [ 1 ]
        , initial  = WFA.Matrix.make s [ ( (), white, 1 ) ]
        , transition = M.fromList
              [ ( (L,U), WFA.Matrix.make s [ (1, red, 1) ] )
              , ( (L,D), WFA.Matrix.make s [ (1, green, 1) ] )
              , ( (R,U), WFA.Matrix.make s [ (1, white, 1) ] )
              , ( (R,D), WFA.Matrix.make s [ (1, blue, 1) ] )
              ]
        , final    = WFA.Matrix.make s [ ( 1, white, () ) ]
        }

writePBM file aut dep = do
    let pic = picture_opt aut dep
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

picture_opt aut dep = 
    let top = 2^dep - 1
        bnd = ((0,0),(top,top))
    in  array bnd $ positions_with_weights aut dep 

positions_with_weights aut dep =
    let initialize v = 
            let res = WFA.Matrix.times ( initial aut ) v
            in  WFA.Matrix.get res ( (),() )
        f path v dep =
            if dep == 0 then [ ( position $ path, initialize v ) ]
            else do
                q <- quads
                let m = M.findWithDefault ( error "pww_opt" ) q $ transition aut
                    v' = WFA.Matrix.times m v
                f ( q : path ) v' ( dep - 1 )
    in  f [] ( final aut ) dep





