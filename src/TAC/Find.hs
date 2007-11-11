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
          , num_combine = 500
          , mutate = mehrfach 3 $ \ p -> do
	        action <- eins [ cut, change_program ]
		action p
          , num_mutate = 500
          , num_compact = 10
          , num_parallel = 1
          }

display vas = sequence_ $ do
    ( v, a ) <- reverse $ take 3 vas
    let p = TAC.Emit.program a
    return $ do
        print $ vcat
	      [ text $ replicate 50 '-'
	      , text "length" <+> toDoc ( length a )
	      , text "cost" <+> toDoc (sum $ map cost a )
	      , text "fitness" <+> toDoc v
	      , text $ unwords $ words $ show a	
	      , fsep $ map text $ words $ show p
	      , text "value" <+> toDoc (value a )
	      , text "transformed length" <+> toDoc ( length p )
	      ]

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


