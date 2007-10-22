module Sortier.Programm.Exec 

( ute
, names
)

where

import Sortier.Programm.Type

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.FiniteMap
import Autolib.TES.Identifier

ute :: Program -> [ Int ] -> Reporter [Int]
ute p input = do
    inform $ text "Eingabe" <+> toDoc input
    let store = listToFM $ zip ( names $ length input )  input
    inform $ text "Belegung" <+> toDoc store
    result <- steps store p
    let output = eltsFM result
    inform $ text "Ausgabe" <+> toDoc output
    return $ output
    
names :: Int -> [ Identifier ]
names w = do x <- take w $ [ 'a' .. ] ; return $ mkunary [x]

type Store = FiniteMap Identifier Int

step :: Store -> Statement -> Reporter Store
step input s = do
    case s of
        Swap x y -> do
            inform $ toDoc s
	    xx <- get input x
	    yy <- get input y
	    let input' = addListToFM input [(x,yy),(y,xx)]
	    inform $ text "Belegung" <+> toDoc input'
	    return $ input'
	If_Greater_Else x y s t -> do
	    xx <- get input x
	    yy <- get input y
	    let check = xx > yy
            inform $ ifgt x y <+> text ":" <+> toDoc check
	    if check then steps input s else steps input t

steps :: Store -> Program -> Reporter Store
steps input ( Sequence xs ) = foldM step input xs

get input x = case lookupFM input x of
    Nothing -> reject $ text "Keine Bindung für Namen" <+> toDoc x
    Just xx -> return xx


