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

make = S.fromList 
     . map ( \ (x,y) -> Position x ( y - 2 ) ) 


data Piece = Piece
           { orig :: Set Position
           , turns :: Int
           , mirrors :: Int
           , shift :: Position
	   -- , delta :: Int
           }
    deriving (Show, Eq, Ord)

points :: Position -> Piece -> Set Position
points s p = 
    let ap 0 f x = x
        ap k f x = f (ap (k-1) f x)
    in  S.map ( ( \ x -> x + s )
             . ap (turns p) turn 
             . ap (mirrors p) mirror
             ) $ orig p

spoints :: Piece -> Set Position
spoints p = points ( shift p ) p

data Figure = Figure 
            { pieces :: [ Piece ]
            , covers :: [ Set Position ]
            }
    deriving (Show, Eq, Ord)


-- | using shifts
figure_shift ps = 
    let cs = map spoints ps
    in  Figure { pieces = ps, covers = cs }

{-
-- | using deltas
figure_delta ps = 
    let f pivot [] = []
	f pivot (p : ps) = 
            let s = points pivot p 
		h = S.toList $ halo s
		i = delta p `mod` length h
	    in  s : f ( h !! i ) ps
	cs = f ( Position 0 0 ) ps
    in  Figure { pieces = ps, covers = cs }

-}

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


{-
roll_delta :: IO Figure
roll_delta = fmap figure_delta $ sequence $ do
    p <- twelve
    return $ do
        t <- randomRIO ( 0, 3 ) 
        m <- randomRIO ( 0, 1 )
	d <- randomRIO ( 0, 20 )
        return $ Piece
               { orig = p
               , turns = t
               , mirrors = m
               -- , shift = Position sx sy
	       , delta = d
               }
-}

roll_shift :: IO Figure
roll_shift = fmap figure_shift $ sequence $ do
    p <- twelve
    return $ do
        t <- randomRIO ( 0, 3 ) 
        m <- randomRIO ( 0, 1 )
        let w = 5
	dx <- randomRIO ( negate w, w )
	dy <- randomRIO ( negate w, w )
        return $ Piece
               { orig = p
               , turns = t
               , mirrors = m
               , shift = Position dx dy
               }


modify :: Piece -> IO Piece
modify p = do
        t <- randomRIO ( 0, 3 ) 
        m <- randomRIO ( 0, 1 )
        return $ p
               { turns = t
               , mirrors = m
               }


area :: Figure -> Int
area = rangeSize . container

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
              q <- neighbours8 p
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
              q <- neighbours4 p
              guard $ S.member q ps
              return q
        start = head $ S.toList $ head $ covers fig
        h = hull f start
    in  S.size h

neighbours8 :: Position -> [ Position ]
neighbours8 p = do
    dx <- [ -1 .. 1 ]
    dy <- [ -1 .. 1 ]
    return $ p +  Position dx dy

neighbours4 :: Position -> [ Position ]
neighbours4 p = do
    dx <- [ -1 .. 1 ]
    dy <- [ -1 .. 1 ]
    guard $ ( dx == 0 ) /= ( dy == 0 )
    return $ p +  Position dx dy


-- | exactly of distance 1
halo :: Set Position -> Set Position
halo s = 
    let h = shull ( \ x -> 
	           S.filter ( \ y -> S.member x s )
	         . S.filter ( \ y -> S.notMember y s )
		 $ S.fromList $ neighbours4 x
		 ) s
    in  S.difference h s
    
hull :: Ord a 
     => ( a -> Set a ) 
     -> a 
     -> Set a
hull f x0 = shull f $ S.singleton x0

shull f todo =
    let h done todo = case S.minView todo of
            Nothing -> done
            Just ( t, odo ) -> 
                let done' = S.union done $ S.singleton t
                    next  = S.filter ( \ x -> S.notMember x done' )
                          $ f t
                in  h done' $ S.union odo next
    in  h S.empty todo



                              






