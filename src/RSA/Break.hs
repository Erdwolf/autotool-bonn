module RSA.Break where

--  $Id$

import RSA.Break.Data
import RSA.Euclid
import Faktor.Prim
import Faktor.Certify ( powmod )

import Data.Typeable

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Ana

data Break = Break deriving ( Show, Typeable )

instance Partial Break Config Integer where

    describe Break conf = vcat
	   [ text "Finden Sie den Klartext für eine RSA-Verschlüsselung mit"
	   , nest 4 $ toDoc conf
	   ]

    initial Break conf  = 
        let (d, n) = public_key conf
	    b = 3
	    xs = based b n
	in  unbased b $ reverse xs

    total Break conf x = do
        let ( d, n ) = public_key conf
	let y = powmod x d n
	inform $ vcat
	       [ text "bei Verschlüsselung von" <+> toDoc x
	       , fsep [ text "erhält man"
		      , toDoc x, text "^", toDoc d, text "=" 
		      , toDoc y, text "mod", toDoc n
		      ]
	       ]
	assert ( y == message conf )
	       $ text "Stimmt das mit vorliegender Nachricht überein?"

break :: Config -> Integer -> Integer
break conf x = 
    let (d, n) = public_key conf
	Just [(p, 1), (q, 1)] = factor n
	phi = pred p * pred q
	(a, b) = euclid phi d
	e = b `mod` phi
    in	powmod x e n

