module Program.Array.Value where

import Autolib.TES.Identifier

import Autolib.Reader
import Autolib.ToDoc


-- | multidimensional array
data Value = Scalar Integer
	   | Row [ Value ]

instance ToDoc Value where
    toDoc v = case v of
        Scalar i -> toDoc i
	Row vs -> dutch_record $ map toDoc vs

instance Reader Value where
    reader = do i <- my_integer ; return $ Scalar i
         <|> do vs <- my_braces $ my_commaSep reader ; return $ Row vs


