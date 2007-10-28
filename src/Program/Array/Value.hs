module Program.Array.Value where

import Autolib.TES.Identifier

import Autolib.Reader
import Autolib.ToDoc

import Data.List ( nub )

-- | multidimensional array
data Value = Scalar Integer
	   | Row [ Value ]

depth :: Value -> [Int]
depth v = case v of
    Scalar i -> []
    Row xs -> let ds :: [[Int]]
		  ds = map depth xs
	      in  case nub ds of
		     [d] -> length xs : d
		     _ -> error $ "varying depths" ++ show ds

instance ToDoc Value where
    toDoc v = case v of
        Scalar i -> toDoc i
	Row vs -> dutch_record $ map toDoc vs

instance Reader Value where
    reader = scalar <|> row

value [] = scalar
value (d : ds) = my_braces $ do
    xs <- replicate_comma d $ value ds
    return $ Row xs

replicate_comma 0 parser = return []
replicate_comma d parser | d > 0 = do
    x <- parser
    xs <- sequence $ replicate ( d-1 ) $ do my_comma ; parser 
    return $ x : xs

scalar = do 
    i <- my_integer
    return $ Scalar i

-- | with unknown depth; determine depth from first element
row = my_braces $ do 
    x <- reader
    xs <- many ( do my_comma ; value ( depth x ) )
    return $ Row $ x : xs


