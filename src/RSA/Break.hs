module RSA.Break where

import RSA.Break.Data
import RSA.Euclid
import Faktor.Prim
import Faktor.Certify ( powmod )

import Data.Typeable

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Ana

import Inter.Types

data RSA_Code_Break = RSA_Code_Break deriving ( Read, Show, Typeable )

instance OrderScore RSA_Code_Break where
    scoringOrder _ = None

instance Partial RSA_Code_Break Config Integer where

    describe RSA_Code_Break conf = vcat
	   [ text "Finden Sie den Klartext für eine RSA-Verschlüsselung mit"
	   , nest 4 $ toDoc conf
	   ]

    initial RSA_Code_Break conf  = 
        let (d, n) = public_key conf
	    b = 3
	    xs = based b n
	in  unbased b $ reverse xs

    total RSA_Code_Break conf x = do
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

instance Measure RSA_Code_Break Config Integer where
    measure RSA_Code_Break c i = 0

break :: Config -> Integer -> Integer
break conf x = 
    let (d, n) = public_key conf
	Just [(p, 1), (q, 1)] = factor n
	phi = pred p * pred q
	(a, b) = euclid phi d
	e = b `mod` phi
    in	powmod x e n

make :: Make
make = direct RSA_Code_Break RSA.Break.Data.example
