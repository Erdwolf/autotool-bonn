module RSA.Break where

--  $Id$

import RSA.Break.Data
import RSA.Euclid
import Faktor.Prim
import Faktor.Certify ( powmod )

import Data.Typeable

import Challenger.Partial
import ToDoc
import Reporter
import Ana

data Break = Break deriving ( Show, Typeable )

instance Partial Break Config Integer where

    describe Break conf = vcat
	   [ text "Finden Sie den Klartext f�r eine RSA-Verschl�sselung mit"
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
	       [ text "bei Verschl�sselung von" <+> toDoc x
	       , fsep [ text "erh�lt man"
		      , toDoc x, text "^", toDoc d, text "=" 
		      , toDoc y, text "mod", toDoc n
		      ]
	       ]
	assert ( y == message conf )
	       $ text "Stimmt das mit vorliegender Nachricht �berein?"

break :: Config -> Integer -> Integer
break conf x = 
    let (d, n) = public_key conf
	Just [(p, 1), (q, 1)] = factor n
	phi = pred p * pred q
	(a, b) = euclid phi d
	e = b `mod` phi
    in	powmod x e n

