module TAC.Find where

import TAC.Data
import qualified TAC.Emit

import Autolib.Genetic
import Autolib.Util.Zufall

import Autolib.ToDoc

conf :: Integer -> Config Program ( Integer, Int )
conf target = Config 
          { fitness = \ p -> 
               let out = abs $ value p - target
		   co = sum $ map cost p
	       in  ( negate out, negate co )
          , threshold = ( 1, 0 )
          , present = display
          , trace   = display
          , size    = 1000
          , generate = some 5
          , combine = \ p q -> do
	       ( px, py ) <- some_split p
	       ( qx, qy ) <- some_split q
	       return $ px ++ qy 
          , num_combine = 1000
          , mutate = \ p -> do
                k <- randomRIO ( 1, 10 )
                mehrfach k ( \ p -> do
	            action <- eins [ cut, change_program, shorten ]
		    action p
                  ) p
          , num_mutate = 1000
          , num_compact = 10
          , num_parallel = 10
          , num_steps = Nothing
          }

display vas = sequence_ $ do
    ( v@(val,c), a ) <- reverse $ take 3 vas
    let p = TAC.Emit.program a
    return $ do
        print $ vcat
	      [ text $ replicate 50 '-'
	      , text "length" <+> toDoc ( length a )
	      , text "cost" <+> toDoc (sum $ map cost a )
	      , text "fitness" <+> toDoc v
	      , if val == 0 then text $ unwords $ words $ show a	else empty
	      , if val == 0 then fsep $ map text $ words $ show p else empty
	      , text "value" <+> toDoc (value a )
	      , text "transformed length" <+> toDoc ( length p )
	      ]

shorten [] = return []
shorten xs = do
    k <- randomRIO ( 0, length xs - 1 )
    let ( pre, this : post ) = splitAt k xs
    return $ pre ++ post

some_split xs = do
    k <- randomRIO ( 0, length xs - 1 )
    return $ splitAt k xs

mehrfach :: Monad m 
	 => Int -> ( a -> m a ) -> ( a ->  m a )
mehrfach 0 action = return 
mehrfach k action = \ x -> action x >>= mehrfach (k-1) action

cut p = do
    i <- randomRIO ( 0, length p `div` 2 )
    j <- randomRIO ( 0, length p `div` 2 )
    return $ take j $ drop i p


