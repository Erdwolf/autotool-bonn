import Autolib.Exp
import Autolib.Exp.Inter
import Autolib.NFA.Minus
import Autolib.NFA.Shortest hiding ( present )
import Autolib.Genetic
import Autolib.Size

import Control.Monad ( guard, forM_ )
import Autolib.Util.Zufall

main = evolve $ conf 42

conf s = 
   Config { fitness = \ x ->
                if Autolib.Size.size x > s then 0
                else 1000 * (1 + miss x) 
                         - Autolib.Size.size x
          , threshold = 1000 * 59
          , present = \ pop -> forM_ ( take 3 pop ) 
                    $ \ (v,x) -> 
                    print ( miss x
                          , x 
                          , Autolib.Size.size x 
                          )
          , trace   = const $ return ()
          , generate = roll s
          , combine = \ s t -> do
                (x,y) <- cross s t
                eins [x,y]
          , mutate  = \ s -> do
                (c,t0) <- contextIO s
                (d,t1) <- contextIO s
                action <- eins 
                       [ roll $ Autolib.Size.size t0
                       , return t1 
                       ]
                t2 <- action
                eins [ s,  c t2 ]
          , num_steps = Nothing
          , num_parallel = 1
          , Autolib.Genetic.size    = 100
          , scheme = Tournament 2
          }

miss :: RX Char -> Int
miss x = 
    let sigma = "a"
        a = inter_det ( std_sigma sigma ) x
        c = complement_det  sigma a
        ws = shortest c
    in  case ws of
            [] -> 0
            w : _ -> length w

roll :: Int -> IO ( RX Char )
roll s = 
    if s <= 1
    then eins [ Letter 'a', Ref "Eps" ]
    else do
        l <- randomRIO ( 0, s-2)
        if l == 0 
           then do
               t <- roll (s-1)
               return $ PowerStar t
           else do
               tl <- roll l
               tr <- roll (s-1-l)
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
          k <- randomRIO ( -1, length xs - 1 )
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
