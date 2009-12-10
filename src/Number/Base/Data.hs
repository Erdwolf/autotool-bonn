{-# LANGUAGE TemplateHaskell #-}

module Number.Base.Data where

import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Reader
import Data.Typeable

import Number.Wert
import Data.Char

data Ziffer = Ziffer Int
    deriving ( Eq, Ord, Typeable )

zehn :: Int
zehn = 10

instance ToDoc Ziffer where
    toDoc (Ziffer z) =
        if (0 <= z) && (z < zehn) then toDoc z
        else if z - zehn <= fromEnum 'Z' - fromEnum 'A' 
             then Autolib.ToDoc.char $ toEnum $ fromEnum 'A' + (z - zehn)
             else error $ "cannot convert digit to character: " ++ show z

instance Reader Ziffer where
    reader = 
            do d <- satisfy isDigit
               my_whiteSpace
               return $ Ziffer $ fromEnum d - fromEnum '0'
        <|> do d <- satisfy isLower
               my_whiteSpace
               return $ Ziffer $ fromEnum d - fromEnum 'a' + zehn
        <|> do d <- satisfy isUpper
               my_whiteSpace
               return $ Ziffer $ fromEnum d - fromEnum 'A' + zehn
     

data Zahl = Zahl { basis :: Int
                 , ziffern :: [ Ziffer ]
                 }
    deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Zahl])

instance Wert_at [ Ziffer ] Integer where
    wert_at b zs = 
        foldl ( \ x (Ziffer y) -> fromIntegral b * x + fromIntegral y ) 0 zs

instance Wert Zahl Integer where
  wert z = wert_at (basis z) (ziffern z)

--------------------------------------------------------------------------

class Range a where range :: Int -> a -> Reporter ()

instance Range [ Ziffer ] where
    range b zs = silent $ do
        inform $ fsep 
               [ text "teste Ziffern", toDoc zs
               , text "bezÃ¼glich der Basis", toDoc b
               ]
        nested 4 $ sequence_ $ do
            z <- zs
            return $ when ( not ( ( Ziffer 0 <= z ) && ( z < Ziffer b ) ) ) 
               $ reject $ fsep 
               [ text "die Ziffer", toDoc z
               , text "liegt nicht im erlaubten Bereich."
               ]


-- local variables:
-- mode: haskell
-- end;
