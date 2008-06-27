{-# language PatternSignatures #-}

import Robots3.Config
import Robots3.Quiz
import Robots3.Data
import Robots3.Nice
import Robots3.Generator

import Inter.Quiz

import Autolib.ToDoc

import Control.Monad ( when )
import Autolib.Util.Wort ( someIO )

p :: RC
p = RC { width = 3
	, num_robots = 5
	, num_targets = 1
	, at_least = 5
	, search_width = 10000
	}


main = searcher 0

searcher k = do
    ( conf :: Config, sol :: [ Zug] ) <- generator Robots3 p undefined
    if ( length sol >= k ) 
        then do
            let sigma = [ '0' .. '9' ] ++ [ 'a' .. 'z' ]
            key <- someIO sigma 5
            print key
            print $ vcat [ text key, nice conf, short sol ]

            appendFile ( "solar-questions.text" ) $ show $
                text key <+> nice conf
            appendFile ( "solar-answers.text" ) $ show $
                text key <+> short sol
        else do
            -- putStr "*"
            return ()
    searcher $ max k $ length sol

short sol = fsep $ do
    m <- sol
    return $ text ( robot m ) <> toDoc ( richtung m )


