-- | Umwandlung von Zahldarstellungen zu verschiedenen Basen

module Number.Base.Central (
      make_fixed 
     , make_quiz
    ) where

--  $Id$

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter

import Inter.Types
import Inter.Quiz
import Data.Typeable
import Autolib.Size
import System.Random

import Number.Wert
import Number.Base.Data
import Number.Base.Param

-------------------------------------------------------------------------------

data Convert = Convert deriving ( Show, Typeable )

instance Partial Convert (Zahl, Int) Zahl where

    describe Convert (z, b) = vcat
	   [ text "Stellen Sie"
           , nest 4 $ toDoc z
           , text "in der Basis" <+> toDoc b <+> text "dar."
	   ]

    initial Convert (z, b) = 
           Zahl { basis = b, ziffern = ziffern z }

    partial Convert (z, b) x = do
         assert ( basis x == b ) 
                $ text "die Basis soll" <+> toDoc b <+> text "sein."
         inform $ text "jede Ziffer soll in [ 0, 1 .. basis - 1 ] liegen."
         silent $ sequence_ $ do
             Ziffer i <- ziffern x
             return $ assert ( 0 <= i && i < b )
                    $ text "falsch für:" <+> toDoc (Ziffer i)

    total Convert (z, b) x = do
         assert ( (wert z :: Integer) == (wert x :: Integer) )
                $ text "Stimmen die Bedeutungen der Zahlen sollen überein?"



express :: Int -> Integer -> Zahl
express b z = 
    let fun 0 = []
        fun z = let (q,r) = divMod z (fromIntegral b)
                in  Ziffer (fromIntegral r) : fun q
    in  Zahl { basis = b
             , ziffern = reverse $ fun z
             }


-- das ist nicht sehr sinnvoll,
-- mir fällt keine aufgabe ein, bei der man das braucht
instance Size Zahl where
    size z = fromIntegral $ basis z *  (length ( ziffern z ))

-------------------------------------------------------------------------------

make_quiz :: Make
make_quiz = quiz Convert Number.Base.Param.p

instance Generator Convert Param (Zahl, Int) where
    generator _ p key = do
        n <- case quelle p of
             Matrikel -> return $ read key
             b @ Bereich {} -> randomRIO ( von b, bis b )
        return ( express ( von_basis p ) n, nach_basis p )

instance Project Convert (Zahl, Int) (Zahl, Int) where
    project Convert (z, b) = (z, b)


make_fixed :: Make
make_fixed = direct Convert ( Zahl { basis = 5
                                   , ziffern = read "[3,1,4,1]"
                                   }
                            , 7 :: Int
                            )

