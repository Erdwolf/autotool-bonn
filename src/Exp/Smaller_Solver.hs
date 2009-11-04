import Autolib.Exp
import Autolib.Exp.Inter
import Autolib.NFA.Minus
import Autolib.NFA.Type hiding ( cross )
import Autolib.NFA.Shortest hiding ( present )
import Autolib.Genetic
import Autolib.Size
import Autolib.Util.Zufall
import Control.Monad ( guard, forM_ )
import System.IO
import System.Environment

main = do
    [ input ] <- getArgs
    let exp = read input
        target = inter std exp
    putStrLn $ unlines [ "find small simple regular expression for"
                    , show exp
                    , "automaton is"
                    , show target
                    ]
    evolve $ conf target

printf x = do print x ; hFlush stdout

sinnlos_times_eps x = sum $ do
    p <- positions x
    Just ( Dot l r ) <- return $ peek p x
    Ref "Eps" <- [ l, r ]
    return 1

sinnlos_union x = sum $ do
    p <- positions x
    Just ( Union l r ) <- return $ peek p x
    guard $ l == r
    return 1

sinnlos_sterne x = sum $ do
    p <- positions x
    Just ( PowerStar y ) <- return $ peek p x
    case y of
        PowerStar _ -> return 1
        Ref "Eps" -> return 1
        _ -> return 0

sinnvoll_sterne x = sum $ do
    p <- positions x
    Just ( PowerStar y ) <- return $ peek p x
    case y of
        Ref _ -> return 0
        PowerStar _ -> return 0
        _ -> return $ Autolib.Size.size y

einzelbuchstaben x = sum $ do
    p <- positions x
    Just ( Letter {} ) <- return $ peek p x
    return 1

weight a =
    sum $ map ( \ w -> 3 ^^ negate ( fromIntegral ( length w ) ) )
        $ take 100
        $ accepted a

conf target =
  let sigma = setToList $ alphabet target
      initial_size = 20
  in 
   Config { fitness = \ x -> negate $ 
                let a = inter std x
                    missing = weight $ minus target a
                    toomuch = weight $ minus a target
                    wrong = toomuch + missing
                in  
                    ( if sinnlos_sterne x > 0 then 10000000000
                    else if sinnlos_times_eps x > 0 then 10000000000
                    else if toomuch > 0 then 1000000
                    else if missing > 0 then 1000 * ( missing + 1 )
                    else 0 )
                    + ( fromIntegral $ Autolib.Size.size x )
          , threshold = 0 :: Double
          , present = \ pop -> forM_ ( take 3 pop ) 
                    $ \ (v,x) -> 
                    printf ( v
                          , x 
                          , Autolib.Size.size x 
                          )
          , trace   = const $ return ()
          , generate = roll sigma initial_size
          , combine = \ s t -> do
                (x,y) <- cross s t
                eins [x,y]
          , mutate  = \ s -> do
                (c,t0) <- contextIO s
                t1 <- roll sigma $ Autolib.Size.size t0
                eins [ c t0, c t1 ]
          , num_steps = Nothing
          , num_parallel = 1
          , Autolib.Genetic.size    = 100
          , scheme = Tournament 2
          }

roll :: [Char] -> Int -> IO ( RX Char )
roll sigma s = 
    if s <= 1
    then do
        eins $ Ref "Eps" : map Letter sigma
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
