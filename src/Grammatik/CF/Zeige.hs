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

-- | zeigt 5 ableitungsbäume
-- und 5 abgeleitete terminalwörter
zeige :: Int
      -> Grammatik 
      -> Reporter [ String ]
zeige cut g = do
    zeige_ableitungen cut g
    -- nur erreichbare und produktive, 
    -- damit das tool nicht sinnlos rumrechnet
    zeige_woerter cut $ reduktion g

-- | zeigt 5 ableitungsbäume
zeige_ableitungen :: Int
      -> Grammatik 
      -> Reporter ()
zeige_ableitungen cut g = inform $ vcat 
	[ text "Hier sind einige ableitbare Wörter"
	, text "(nicht notwendig nur aus Terminalzeichen)"
	, text "mit dazugehörigen Ableitungsbäumen:"
        , text ""
        , rundown 5 $ map ( \ b -> ( yield b, b ) ) 
	            $ take cut $ baums g
	]

-- | zeigt 5 ableitbare terminalwörter
-- und liefert (at most) cut ableitbare terminalwörter
zeige_woerter :: Int
      -> Grammatik 
      -> Reporter [ String ]
zeige_woerter cut g = do
    if ( not $ startsymbol g `elementOf` variablen g )
       then do
	    inform $ text "Es sind keine Terminalwörter ableitbar."
	    return []
       else do
            let wbs = do
		     b <- take cut $ baums g
                     let w = yield b
		     guard $ all (`elementOf` terminale g) w
		     return (w, b)
	    inform $ vcat
		   [ text "Einige ableitbare Terminalwörter sind:"
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

