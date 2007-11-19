module Rushhour.Move where

import Rushhour.Data

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.FiniteMap

import Data.Array

onboard i (x,y) = 
    and [ negate ( width i ) <= x, x <= width i
	, negate ( height i ) <= y, y <= height i
	]

final :: Instance -> Reporter ()
final i = do
    inform $ text "Anordnung" <+> nice i
    assert ( solved i ) $ hsep 
           [ text "Auto", toDoc ( target i ), text "kann ausparken?" ]

solved :: Instance -> Bool
solved i = and $ do
    let occ = occupied i
    ( name, car ) <- fmToList $ cars i
    guard $ name == target i
    let (x, y) = position car
	( dx, dy ) = offset $ orientation car
    p <- takeWhile ( onboard i ) $ do
	    d <- [ extension car .. ]
	    return ( x + d * dx, y + d * dy )
    return $ null $ occ ! p

executes :: Instance ->  [Zug] -> Reporter Instance
executes = foldM exec

exec :: Instance -> Zug -> Reporter Instance
exec i z @ ( name, d ) = do
    inform $ text "Anordnung" <+> nice i
    inform $ hsep [ text "ziehe", toDoc z ]
    car <- case lookupFM ( cars i ) name of
        Nothing -> reject $ text "dieses Auto gibt es nicht"
	Just car -> return car
    let o @ ( ox , oy  ) = position car
	( dx, dy ) = offset $ orientation car
	n @ ( nx, ny ) = ( ox + d * dx, oy + d * dy )
    inform $ hsep [ text "von", toDoc o, text "nach", toDoc n ]

    let e = extension car
	bnd = ( ( min ox nx, min oy ny )
	      , ( (e-1) * dx + max ox nx, (e-1) * dy + max oy ny ) 
	      )
    let occ = occupied $ i `without` name
    sequence $ do
	q <- range bnd
        return $ do
	    when ( not $ inRange ( bounds occ ) q ) $ reject $ hsep
	        [ text "benutze Position", toDoc q, text "ist nicht auf dem Brett." ]
	    case occ ! q of
	        [] -> return ()
                bs -> reject $ hsep [ text "benutzte Position", toDoc q
				, text "ist blockiert durch", hsep $ map toDoc bs
				]	
    let car' = car { position = n } 
    let i' = i { cars = addToFM ( cars i ) name car' }
    return i'
    
without i name = i { cars = delFromFM ( cars i ) name}



