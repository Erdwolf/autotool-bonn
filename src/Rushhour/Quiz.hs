module Rushhour.Quiz where

import qualified  Rushhour.Data as I
import qualified Rushhour.Config as C
import Rushhour.Solve

import Autolib.Util.Zufall
import Autolib.Util.Sort
import Autolib.FiniteMap
import Autolib.ToDoc

import Data.Array
import Control.Monad

import System.IO

roll0 c = do
    ( i, (k,zs) : _ ) <- do
          i <- create c 
          hPutStrLn stderr $ show $ I.present i
          let kzs = solutions ( C.max_solution c ) ( C.max_search_width c ) i 
          hPutStrLn stderr $ show $  take 5 $ map (length . snd ) kzs
          return ( i, kzs ) 
       `repeat_until` \ ( i, kzs ) -> 
          case kzs of
                [] -> False
                (k , zs) : _ -> length zs >= C.min_solution c
    return ( i, zs )

roll2 c = do
    i <- create_solvable c
    let kzs = reachables1 ( C.max_solution c ) ( C.max_search_width c ) i
    sequence_ $  do 
       (k, zs) <- kzs
       let sols = solutions ( C.max_solution c ) ( C.max_search_width c ) k
       return $ case sols of
            [] -> hPutStr stderr "."
            ( j, ms ) : _ -> do
                print $ vcat [ I.present k , toDoc ms, toDoc ( length ms ) ]

roll1 c = do
    i <- create_only_target c
    handler c i

create c =     do   
    i <- create_only_target c
    add_cars c i


create_solvable c = create c `repeat_until` solvable c

solvable c i = not 
    $ null $ solutions ( C.max_solution c ) ( C.max_search_width c ) i

names = do x <- ['A' .. ] ; return $ read [x]

handler c i = do
    hPutStrLn stderr $ show $ I.present i
    let zks = next i
    hPutStrLn stderr $ show $ length zks
    let sols = solutions ( C.max_solution c ) ( C.max_search_width c ) i 
    if null sols 
        then do
                hPutStrLn stderr "unsolvable"
                i <- remove_car i
                handler c i
        else case sols of
            (k,zs) : _ -> do
                let l = length zs
                hPutStrLn stderr $ "solution of length " ++ show l
                if l >= C.min_solution c 
                    then return i
                    else adder c i

adder c i = do
     let free n = not $ n `elem` keysFM ( I.cars i )
         name = head $ filter free names
     i <- add_car c i name 
     handler c i

create_only_target :: RandomC m
       => C.Config 
       -> m I.Instance
create_only_target c = do
    o <- eins [ minBound .. maxBound ]
    p <- case o of
        I.Vertical -> do
            x <- randomRIO ( negate $ C.height c  , C.height c )
            return (x, negate $ C.width c )
        I.Horizontal -> do
            y <- randomRIO ( negate $ C.width c  , C.width c )
            return (negate $ C.height c, y )
    e <- randomRIO ( C.min_extension c, C.max_extension c )
    let car = I.Car { I.extension = e, I.position = p, I.orientation = o }
        name = read "X"
    let i = I.Instance 
	  { I.width = C.width c
	  , I.height = C.height c
	  , I.cars = listToFM [ ( name, car ) ]
          , I.target = name
	  }
    return i    

pick_target i = do
    t <- eins $ keysFM $ I.cars i
    return $ i { I.target = t }

add_cars c i = 
    let names = do x <- [ 'A' .. ] ; return $ read [ x ]
    in  foldM ( add_car c ) i $ take ( C.num_cars c ) names
    
positions car = do
    let ( dx, dy ) = I.offset $ I.orientation car
    d <- [ 0 .. I.extension car - 1 ]
    let (x,y) = I.position car
    return (x+d*dx, y+d*dy)

remove_car i = 
    case filter ( not . ( == I.target i ) ) $ keysFM $ I.cars i of
        [] -> return i
        ns -> do 
          name <- eins ns
          return $ i { I.cars = delFromFM ( I.cars i ) name }

add_car c i name = do
    let emin = C.min_extension c
    case I.spaces emin i of
       [] -> return i
       sp -> do
           let third (x,y,z) = z
           ( p, o, emax ) <- vorderes $ sortBy ( negate . third) sp
           e <- -- return $ min ( C.max_extension c ) emax 
               randomRIO ( emin, min ( C.max_extension c ) emax )
           let car = I.Car 
                   { I.orientation = o, I.extension = e, I.position = p }
           let i' = i  { I.cars = addToFM ( I.cars i ) name car }
           when ( not $ I.consistent i' ) $ error $ show $ vcat
                [ text "before:" <+> I.present i
                , text "add   :" <+> toDoc (name, car)
                , text "after :" <+> I.present i'
                ]
           return $ i'

vorderes [x] = return x
vorderes (x : xs) = do
    i <- randomRIO ( 0 :: Int, 1 )
    if 0 == i 
       then return x
       else vorderes xs

add_car0 c i name = do
    let occ = I.occupied i
        free car = and $ do
	    p <- positions car
	    return $ inRange ( bounds occ ) p  && null ( occ ! p )
    car <- do
        e <- randomRIO ( C.min_extension c, C.max_extension c )
        o <- eins [ I.Horizontal, I.Vertical ]
        let ( dx, dy ) = I.offset o
        let ( ex, ey ) = ( e * dx, e * dy )
        x <- randomRIO ( negate ( I.width i ), I.width i - ex + 1 )
        y <- randomRIO ( negate ( I.height i ), I.height i - ey + 1 )
        return $ I.Car 
               { I.orientation = o, I.extension = e, I.position = (x,y) }
      `repeat_until` free
    return $ i { I.cars = addToFM ( I.cars i ) name car }

    