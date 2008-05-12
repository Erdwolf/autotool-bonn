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
       
a :: WFA Char Int RGB 
a = let s = WFA.RGB.maxplus
    in  WFA 
        { WFA.Type.semiring = s
        , alphabet = S.fromList "lrud"
        , states   = S.fromList [ 1,2 ]
        , initial  = M.fromList [ ( 1, blue ) ]
        , transition = M.fromList
              [ ( 'l', WFA.Matrix.make s [ (1, white, 2) ] )
              , ( 'r', WFA.Matrix.make s [ (1, white, 2) ] )
              , ( 'u', WFA.Matrix.make s [ (1, white, 2) ] )
              , ( 'd', WFA.Matrix.make s [ (1, white, 2) ] )
              ]
        , final    = M.fromList [ ( 2, red ) ]
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


type Path = [ Char ]

paths :: Int -> [ Path ]
paths 0 = return []
paths d | d > 0 = do
    h <- "lr"
    v <- "ud"
    rest <- paths ( d - 1 )
    return $ h : v : rest

type Point = ( Int, Int )

position :: Path -> Point
position p =
    let f (x,y) [] = (x, y)
        f (x,y) (h : v : rest) =
            let hh = case h of 'l' -> 1 ; 'r' -> 0
                vv = case v of 'u' -> 1 ; 'd' -> 0
            in  f ( 2 * x + hh, 2 * y + vv ) rest
    in  f (0,0) p

picture aut dep =
    let top = 2^dep - 1
        bnd = ((0,0),(top,top))
    in  array bnd $ do
            p <- paths dep
            return ( position p, weight aut p )

