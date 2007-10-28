{-# OPTIONS -fglasgow-exts #-}

module Program.Array.Value where

import Autolib.TES.Identifier

import Autolib.Reader
import Autolib.ToDoc

import Autolib.Util.Zufall

import Data.List ( nub )
import Data.Typeable

-- | multidimensional array
data Value = Scalar Integer
	   | Row [ Value ]
    deriving ( Eq, Typeable )

depth :: Value -> [Int]
depth v = case v of
    Scalar i -> []
    Row xs -> let ds :: [[Int]]
		  ds = map depth xs
	      in  case nub ds of
		     [d] -> length xs : d
		     _ -> error $ "varying depths" ++ show ds


get :: Value -> [Integer] -> Integer
get ( Scalar i ) [] = i
get ( Row xs ) ( p : ps ) = get ( xs !! fromIntegral p ) ps


roll :: RandomC m
     => Int -- ^ depth
     -> Int -- ^ size
     -> m Value
roll d s = do
    depth <- roll_depth d s
    roll_for_depth depth ( 0, fromIntegral s )

roll_depth :: RandomC m
	   => Int -- ^ depth
	   -> Int -- ^ total size
	   -> m [ Int ]
roll_depth 0 s = return []
roll_depth 1 s = do
    f <- randomRIO ( s `div` 2, s )
    return [f]
roll_depth d s | d > 1 = do
    let top = round $ ( fromIntegral s ) ** ( 1 / fromIntegral d )
    f <- randomRIO ( 2, top )
    fs <- roll_depth ( d - 1 ) ( max 1 $ s `div` f )
    return $ f : fs


roll_for_depth :: RandomC m
	   => [ Int ] 
	   -> ( Integer, Integer )
	   -> m Value
roll_for_depth [] bnd = do
    i <- randomRIO bnd
    return $ Scalar i
roll_for_depth (d : ds) bnd = do
    vs <- sequence $ replicate d $ roll_for_depth ds bnd
    return $ Row vs


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


