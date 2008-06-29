module Pentomino.Cover where

import Pentomino.Position hiding ( x, y )
import qualified Pentomino.Position as P

import Autolib.ToDoc

import Data.Set ( Set )
import qualified Data.Set as S

import Control.Monad  ( guard )
import Data.Ix (inRange, rangeSize )
import Data.Array

import System.Random

twelve = [ f,i,l,n,p,t,u,v,w,x,y,z ]

f = make [ (1,0),(0,1),(1,1),(1,2),(2,2) ]
i = make [ (0,0),(0,1),(0,2),(0,3),(0,4) ]
l = make [ (0,0),(1,0),(0,1),(0,2),(0,3) ]
n = make [ (0,0),(0,1),(1,1),(1,2),(1,3) ]
p = make [ (0,0),(0,1),(0,2),(1,1),(1,2) ]
t = make [ (1,0),(1,1),(1,2),(0,2),(2,2) ]
u = make [ (0,0),(0,1),(1,0),(2,0),(2,1) ]
v = make [ (0,0),(0,1),(0,2),(1,0),(2,0) ]
w = make [ (0,0),(0,1),(1,1),(1,2),(2,2) ]
x = make [ (1,0),(1,1),(0,1),(2,1),(1,2) ]
y = make [ (0,0),(0,1),(0,2),(0,3),(1,2) ]
z = make [ (0,0),(1,0),(1,1),(1,2),(2,2) ]

make = S.fromList . map ( \ (x,y) -> Position x y ) 


data Piece = Piece
           { orig :: Set Position
           , turns :: Int
           , mirrors :: Int
           , shift :: Position
           }
    deriving (Show, Eq, Ord)

points :: Piece -> Set Position
points p = 
    let ap 0 f x = x
        ap k f x = f (ap (k-1) f x)
    in  S.map ( ( \ x -> x + shift p )
             . ap (turns p) turn 
             . ap (mirrors p) mirror
             ) $ orig p

data Figure = Figure 
            { pieces :: [ Piece ]
            , covers :: [ Set Position ]
            }
    deriving (Show, Eq, Ord)

instance ToDoc Figure where 
    toDoc = text . show


form :: Figure -> Doc
form f =  
    let bnd @ ((l,u),(r,o)) = container f
        a   = accumArray ( flip const ) '.' bnd $ do
              ( tag, c ) <- zip [ 'a' .. ] $ covers f
              x <- S.toList $ c
              return ( (P.x x, P.y x), tag )
    in  vcat $ do
          y <- reverse [ u .. o ]
          return $ text $ do
              x <- [ l .. r ]
              [ a ! (x,y), ' ' ]

figure ps = Figure 
          { pieces = ps
          , covers = map points ps
          }

roll :: IO Figure
roll = fmap figure $ sequence $ do
    p <- twelve
    return $ do
        t <- randomRIO ( 0, 3 ) 
        m <- randomRIO ( 0, 1 )
        sx <- randomRIO ( 0, 15 )
        sy <- randomRIO ( 0, 15 )
        return $ Piece
               { orig = p
               , turns = t
               , mirrors = m
               , shift = Position sx sy
               }

modify :: Piece -> IO Piece
modify p = do
        t <- randomRIO ( 0, 3 ) 
        m <- randomRIO ( 0, 1 )
        sx <- randomRIO ( -1, 1 )
        sy <- randomRIO ( -1, 1 )
        return $ Piece
               { orig = orig p
               , turns = t
               , mirrors = m
               , shift = shift p + Position sx sy
               }

container :: Figure -> ((Int,Int),(Int,Int))
container fig = 
    let ps = S.unions $ covers fig
        ls = S.toList ps
        l  = minimum $ map P.x ls
        r  = maximum $ map P.x ls
        u  = minimum $ map P.y ls
        o  = maximum $ map P.y ls
    in  ((l-1,u-1),(r+1,o+1))

-- | number of unreachable points
unreach :: Figure -> Int
unreach fig = 
    let ps = S.unions $ covers fig
        bnd @ ((l,u),(r,o)) = container fig
        f p = S.fromList $ do
              dx <- [ -1 .. 1 ]
              dy <- [ -1 .. 1 ]
              let q = p +  Position dx dy
              guard $ S.notMember q ps
              guard $ inRange bnd ( P.x q, P.y q )
              return q
        h = hull f $ Position (l-1) (u-1)
    in  rangeSize bnd - S.size h

-- | number of reachables (from first )
reach ::  Figure -> Int
reach fig = 
    let ps = S.unions $ covers fig
        f p = S.fromList $ do
              dx <- [ -1 .. 1 ]
              dy <- [ -1 .. 1 ]
              let q = p +  Position dx dy
              guard $ S.member q ps
              return q
        start = head $ S.toList $ head $ covers fig
        h = hull f start
    in  S.size h

hull :: Ord a 
     => ( a -> Set a ) 
     -> a 
     -> Set a
hull f x0 = 
    let h done todo = case S.minView todo of
            Nothing -> done
            Just ( t, odo ) -> 
                let done' = S.union done $ S.singleton t
                    next  = S.filter ( \ x -> S.notMember x done' )
                          $ f t
                in  h done' $ S.union odo next
    in  h S.empty ( S.singleton x0 )


                              






