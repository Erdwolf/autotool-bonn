module Program.Array.Semantics where

import Program.Array.Statement
import Program.Array.Expression
import Program.Array.Value

import Autolib.Reporter
import Autolib.FiniteMap
import Autolib.ToDoc
import Autolib.TES.Identifier

import Data.Ix

type Environment = FiniteMap Identifier Value

execute :: Environment
	-> Statement
	-> Reporter Environment
execute env st = case st of
    Assign target @ ( Access name indices ) exp -> do
	value <- eval env exp
        previous <- look env name
	next <- update env previous indices value
	return $ addToFM env name next


eval :: Environment
     -> Expression 
     -> Reporter Integer
eval env exp = case exp of
    Literal i -> return i
    Binary op x y -> do
        a <- eval env x
	b <- eval env y
	let f = case op of
	        Add -> (+)
		Subtract -> (-)
		Multiply -> (*)
		Divide-> div
        return $ f a b
    Reference p -> dereference env p

look env name = 
        case lookupFM env name of
	    Nothing -> reject $ text "unbekannter Name" <+> toDoc name
	    Just value -> return value

dereference env (Access name indices) = do
    value <- look env name
    access env value indices

access :: Environment
       -> Value
       -> [ Expression ]
       -> Reporter Integer
access env ( Scalar i ) [] = return i
access env v @ ( Row vs ) ( p : ps ) = do
    q <- eval env p
    let bnd = ( 0, fromIntegral $ length vs - 1 )
    when ( not $ inRange bnd q ) $ reject 
	 $ hsep [ text "Index", toDoc p 
		, text "Wert", toDoc q
		, text "nicht im erlaubten Bereich", toDoc bnd
		]
    access env ( vs !! fromIntegral q ) ps
access env v ps = reject $ vcat
    [ text "unpassende Dimensionen:"
    , text "Wert" <+> toDoc v
    , text "Indices" <+> toDoc ps
    ]

update :: Environment
       -> Value
       -> [ Expression ]
       -> Integer
       -> Reporter Value
update env ( Scalar i ) [] new = return $ Scalar new
update env v @ ( Row vs ) ( p : ps ) new = do
    q <- eval env p
    let bnd = ( 0, fromIntegral $ length vs - 1 )
    when ( not $ inRange bnd q ) $ reject 
	 $ hsep [ text "Index", toDoc p 
		, text "Wert", toDoc q
		, text "nicht im erlaubten Bereich", toDoc bnd
		]
    update env ( vs !! fromIntegral q ) ps new
update env v ps new = reject $ vcat
    [ text "unpassende Dimensionen:"
    , text "Wert" <+> toDoc v
    , text "Indices" <+> toDoc ps
    ]


 
    