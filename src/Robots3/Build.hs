{-# LANGUAGE ScopedTypeVariables #-}

import Robots3.Config
import Robots3.Quiz
import Robots3.Data
import Robots3.Nice
import Robots3.Generator
import Robots3.Solver ( shortest', reachables )

import Robots3.Form

import Inter.Quiz

import Autolib.ToDoc
import Autolib.Util.Wort ( someIO )

import Data.Map ( Map )
import qualified Data.Map as M

import Data.Set ( Set )
import qualified Data.Set as S

import Data.List ( sort )

import Control.Monad ( when, guard  )

main2 = grid 0

grid top = do
    let pos = do 
		let w = 10
		x <- [ negate w .. w ]
		y <- [ negate w .. w ]
		guard $ abs (x - y) < 3
		return $  Position x y 
    conf <- someconf 30 1 pos
    let confss = reachables conf
    when False $ print $ vcat
	[ toDoc conf
	, nice conf
	]
    mapM_ print $ drop top $ zip [ 0 .. ] $ map length confss

    when ( length confss >= top ) $ do
	let far = head $ last confss
	let confblock = Robots3.Config.make ( robots conf ) ( map position $ robots far )
	    conf' = confblock
	print $ vcat
		[ toDoc conf'
		, form conf'
		, nice conf'	
		, toDoc $ length confss
		]
        let moves = shortest' ( 10^4 ) conf'
	print $ vcat [ toDoc $ map length moves, toDoc moves ]
    grid $ max top $ length confss

{-
    let sol = shortest' ( 10 ^ 5 ) conf
    print $ vcat
	[ toDoc $ length sol
	, toDoc sol
	]
-}

-- | map to earliest occurence
distances :: [[Config]] -> Map Position Int
distances confss = M.fromListWith min $ do
    ( d, confs ) <- zip [ 0 .. ] confss
    conf <- confs
    p <- positions conf
    return ( p, d )

farthest confss 
	= reverse 
	. sort 
	. map ( \ (p,d) -> (d,p) )
	. M.toAscList 
	$ distances confss 
    

ex :: Config
ex = Robots3.Config.make [ Robot
           { name = "A"
           , position = Position
                            { x = -3 , y = 6 }
           }
     , Robot
           { name = "B"
           , position = Position
                            { x = -3 , y = 9 }
           }
     , Robot
           { name = "C"
           , position = Position
                            { x = 9 , y = 6 }
           }
     , Robot
           { name = "D"
           , position = Position
                            { x = -3 , y = 0 }
           }
     , Robot
           { name = "E"
           , position = Position
                            { x = 9 , y = 9 }
           }
     , Robot
           { name = "F"
           , position = Position
                            { x = -6 , y = 0 }
           }
     , Robot
           { name = "G"
           , position = Position
                            { x = 0 , y = 6 }
           }
     , Robot
           { name = "H"
           , position = Position
                            { x = -9 , y = -9 }
           }
     ] [ Position
             { x = 0 , y = -9 }
       ]




p :: RC
p = RC { width = 4
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
        else do
            -- putStr "*"
            return ()
    searcher $ max k $ length sol

short sol = fsep $ do
    m <- sol
    return $ text ( robot m ) <> toDoc ( richtung m )


