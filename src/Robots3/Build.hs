{-# language PatternSignatures #-}

import Robots3.Config
import Robots3.Quiz
import Robots3.Data
import Robots3.Nice
import Robots3.Generator

import Robots3.Form

import Inter.Quiz

import Autolib.ToDoc

import Control.Monad ( when )
<<<<<<< Build.hs
import System.Random
=======
import Autolib.Util.Wort ( someIO )
>>>>>>> 1.5

p :: RC
<<<<<<< Build.hs
p = RC { width = 6
=======
p = RC { width = 4
>>>>>>> 1.5
	, num_robots = 5
	, num_targets = 1
	, at_least = 5
	, search_width = 100000
	}


main = searcher 0

searcher k = do
    ( conf :: Config, sol :: [ Zug] ) <- generator Robots3 p undefined
    if ( length sol >= k ) 
        then do
<<<<<<< Build.hs
            let msg = show $ vcat
	           [ toDoc $ length sol
	       	   , toDoc conf
                   , nice conf
                   , toDoc  sol
                   ]
            n :: Integer <- randomRIO ( 0, 10^10 ) 
            putStrLn msg
            writeFile ( "robots-" ++ show n ++ ".conf" ) msg
=======
            let sigma = [ '0' .. '9' ] ++ [ 'a' .. 'z' ]
            key <- someIO sigma 5
            print key

            print $ vcat 
		  [ text key, nice conf, short sol 
		  , form conf 
		  ]
	    when (False) $ do
                appendFile ( "solar-questions.text" ) $ show $
                    text key <+> nice conf
                appendFile ( "solar-answers.text" ) $ show $
                    text key <+> short sol

>>>>>>> 1.5
        else do
            -- putStr "*"
            return ()
    searcher $ max k $ length sol

short sol = fsep $ do
    m <- sol
    return $ text ( robot m ) <> toDoc ( richtung m )


