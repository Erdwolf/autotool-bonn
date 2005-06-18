module Grammatik.CF.Zeige

-- -- $Id$

( zeige
)

where

import Grammatik.Type
import Grammatik.CF.Baum

import Grammatik.Reduziert

import Autolib.Set

import Autolib.Util.Wort

import Autolib.Reporter
import Autolib.ToDoc


---------------------------------------------------------------------------

-- | zeigt 5 ableitungsb�ume
-- und 5 abgeleitete terminalw�rter
zeige :: Int
      -> Grammatik 
      -> Reporter [ String ]
zeige cut g = do
    zeige_ableitungen cut g
    -- nur erreichbare und produktive, 
    -- damit das tool nicht sinnlos rumrechnet
    zeige_woerter cut $ reduktion g

-- | zeigt 5 ableitungsb�ume
zeige_ableitungen :: Int
      -> Grammatik 
      -> Reporter ()
zeige_ableitungen cut g = inform $ vcat 
	[ text "Hier sind einige ableitbare W�rter"
	, text "(nicht notwendig nur aus Terminalzeichen)"
	, text "mit dazugeh�rigen Ableitungsb�umen:"
        , text ""
        , rundown 5 $ map ( \ b -> ( yield b, b ) ) 
	            $ take cut $ baums g
	]

-- | zeigt 5 ableitbare terminalw�rter
-- und liefert (at most) cut ableitbare terminalw�rter
zeige_woerter :: Int
      -> Grammatik 
      -> Reporter [ String ]
zeige_woerter cut g = do
    if ( not $ startsymbol g `elementOf` variablen g )
       then do
	    inform $ text "Es sind keine Terminalw�rter ableitbar."
	    return []
       else do
            let wbs = do
		     b <- take cut $ baums g
                     let w = yield b
		     guard $ all (`elementOf` terminale g) w
		     return (w, b)
	    inform $ vcat
		   [ text "Einige ableitbare Terminalw�rter sind:"
		   , text ""
		   , rundown 5 $ wbs
		   ]
	    return $ map fst wbs

-------------------------------------------------------------------------

rundown :: Int -> [( String, Baum )] -> Doc    
rundown cut wbs = nest 4 $ besides $ do
     (w, b) <- pick cut wbs
     return $ toDoc (yield b, toDoc b)

pick :: Int -> [a] -> [a]
pick n xs = 
    let picker k xs | k > n || null xs = []
        picker k (x : xs) = x : picker (succ k) (drop k xs)
    in  picker 0 xs

