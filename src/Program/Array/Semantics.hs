module Program.Array.Semantics where

import Program.General.Environment ( Environment )
import qualified Program.General.Environment as E

import Program.Array.Program
import Program.Array.Statement
import Program.Array.Expression
import Program.Array.Operator
import Program.Array.Value

import Autolib.Reporter
import Autolib.FiniteMap
import Autolib.ToDoc
import Autolib.TES.Identifier

import Data.Ix

single :: Environment Value
	-> Statement
	-> Reporter ( Environment Value )
single env st = do
    inform $ text "Anweisung:" <+> toDoc st
    nested 4 $ case st of
        Assign target @ ( Access name indices ) exp -> do
	    value <- eval env exp
            previous <- look env name
            inform $ text "vorher :" 
		   <+> toDoc ( Declare name ( depth previous ) previous )
	    next <- update env previous indices value
            inform $ text "nachher:" 
		   <+> toDoc ( Declare name ( depth previous ) next  )
	    return $ E.add env name next
        Declare name depth value -> do
            case E.lookup env name of
	        Just v -> reject $ hsep [ text "Name" , toDoc name
				    , text "ist schon deklariert,"
				    , text "Wert ist", toDoc v
				    ]
	        Nothing -> do
	            return $ E.add env name value

eval :: Environment Value
     -> Expression 
     -> Reporter Integer
eval env exp = case exp of
    Literal i -> return i
    Binary op x y -> do
        a <- eval env x
	b <- eval env y
        return $ Program.Array.Operator.semantics op a b
    Reference p -> dereference env p

look env name = 
        case E.lookup env name of
	    Nothing -> reject $ text "unbekannter Name" <+> toDoc name
	    Just value -> return value

dereference env (Access name indices) = do
    value <- look env name
    access env value indices

access :: Environment Value
       -> Value
       -> [ Expression ]
       -> Reporter Integer
access env ( Scalar i ) [] = return i
access env v @ ( Row vs ) ( p : ps ) = do
    q <- eval env p
    let bnd = ( 0, fromIntegral $ length vs - 1 )
    when ( not $ inRange bnd q ) $ reject 
	 $ hsep [ text "Index:", toDoc p, comma
		, text "Wert:", toDoc q, comma
		, text "nicht im erlaubten Bereich:", toDoc bnd
		]
    access env ( vs !! fromIntegral q ) ps
access env v ps = reject $ vcat
    [ text "unpassende Dimensionen:"
    , text "Wert" <+> toDoc v
    , text "Indices" <+> toDoc ps
    ]

update :: Environment Value
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
    let ( pre, this : post ) = splitAt ( fromIntegral q ) vs
    that <- update env this ps new
    return $ Row $ pre ++ that : post

update env v ps new = reject $ vcat
    [ text "unpassende Dimensionen:"
    , text "Wert" <+> toDoc v
    , text "Indices" <+> toDoc ps
    ]

patches :: Environment Value
        -> ( Integer, Integer )
        -> [ Statement ]
patches env bnd = do
    ( k, v ) <- E.contents env
    p <- positions v
    w <- range bnd
    return $ Assign ( Access k $ map Literal p ) ( Literal w )


