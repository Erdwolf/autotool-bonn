import Autolib.Exp
import Autolib.Exp.Inter
import Autolib.NFA.Minus
import Autolib.NFA.Shortest hiding ( present )
import Autolib.Genetic
import Autolib.Size
import Autolib.Util.Zufall
import Control.Monad ( guard, forM_ )
import System.IO

main = evolve $ conf "ab" 42

printf x = do print x ; hFlush stdout

sinnlos_sterne x = sum $ do
    p <- positions x
    Just ( PowerStar y ) <- return $ peek p x
    case y of
        Ref _ -> return 1
        PowerStar _ -> return 1
        _ -> return 0

sinnvoll_sterne x = sum $ do
    p <- positions x
    Just ( PowerStar y ) <- return $ peek p x
    case y of
        Ref _ -> return 0
        PowerStar _ -> return 0
        _ -> return 1

conf sigma s = 
   Config { fitness = \ x ->
                if Autolib.Size.size x > s then 0
                else if sinnlos_sterne x > 0 then 0
                else if sinnvoll_sterne x == 0 then 0
                else 1000 * (1 + miss sigma x) 
                        - Autolib.Size.size x
          , threshold = 1000 * 100
          , present = \ pop -> forM_ ( take 3 pop ) 
                    $ \ (v,x) -> 
                    printf ( miss sigma x
                          , x 
                          , Autolib.Size.size x 
                          )
          , trace   = const $ return ()
          , generate = roll sigma s
          , combine = \ s t -> do
                (x,y) <- cross s t
                eins [x,y]
          , mutate  = \ s -> do
                (s0, s1) <- cross s s
                s <- eins [s0, s1]
                (c,t0) <- contextIO s
                t1 <- roll sigma $ Autolib.Size.size t0
                eins [ c t0, c t1 ]
          , num_steps = Nothing
          , num_parallel = 1
          , Autolib.Genetic.size    = 1000
          , scheme = Tournament 3
          }

miss :: [Char] -> RX Char -> Int
miss sigma x = 
    let a = inter_det ( std_sigma sigma ) x
        c = complement_det  sigma a
        ws = shortest c
    in  case ws of
            [] -> 0
            w : _ -> length w

roll :: [Char] -> Int -> IO ( RX Char )
roll sigma s = 
    if s <= 1
    then do
        x <- eins sigma
        eins [ Letter x, Ref "Eps", Ref "Sigma", Ref "All" ]
    else do
        l <- randomRIO ( 0, s-2)
        if l == 0 
           then do
               t <- roll sigma (s-1)
               return $ PowerStar t
           else do
               tl <- roll sigma l
               tr <- roll sigma (s-1-l)
               f <- eins [ Dot, Union ]
               return $ f tl tr

type Position = [ Int ]

class Tree t where
    explode :: t -> ( [t] -> t, [ t ] )

instance Tree ( RX c ) where
    explode t = case t of
        Dot l r -> ( \ [x,y] -> Dot x y, [ l, r ] )
        Union l r -> ( \ [x,y] -> Union x y, [ l, r ] )
        PowerStar x -> ( \ [x] -> PowerStar x, [x] )
        _ -> ( \ [] -> t , [] )

positions :: Tree t => t -> [ Position ]
positions t = [] : do
    let ( f, xs ) = explode t 
    (k, x) <- zip [0..] xs
    p <- positions x
    return $ k : p

contextIO :: Tree t => t -> IO ( t -> t, t )
contextIO t = do
    let ( f, xs ) = explode t
    if null xs 
       then return ( id, t )
       else do
          k <- randomRIO ( -length xs + 1, length xs - 1 )
          if k < 0 
             then return ( id, t )
             else do
                 let ( pre, this : post ) = splitAt k xs
                     plug x = f $ pre ++ x : post
                 ( c, s ) <- contextIO this
                 return ( plug . c , s )

peek :: Tree t => Position -> t -> Maybe t
peek [] t = return t
peek (p : ps) t = do
    let (f, xs) = explode t
    guard $ p < length xs
    peek ps $ xs !! p

poke :: Tree t => ( Position, t ) -> t -> Maybe t
poke ( [], s ) t = return s
poke ( p : ps, s ) t = do
    let ( f, xs ) = explode t
    guard $ p < length xs
    let ( pre, this : post ) = splitAt p xs
    this' <- poke ( ps, s ) this
    return $ f $ pre ++ this' : post

cross :: Tree t => t -> t -> IO ( t, t )
cross s t = do
    (cons, subs) <- contextIO s
    (cont, subt) <- contextIO t
    return ( cons subt, cont subs )
