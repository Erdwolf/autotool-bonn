{-# OPTIONS -fno-monomorphism-restriction #-}

module TAC.Data where

import Autolib.Reader
import Autolib.ToDoc
import Autolib.FiniteMap

import Autolib.Util.Zufall

type Program = [ Statement ]

data Statement 
     = Constant Int Int
     | Add Int Int Int
     | Mul Int Int Int
   deriving ( Eq, Ord )

{-! for Statement derive: Reader, ToDoc !-}

some :: Int -> IO Program
some l = sequence $ replicate l $ do
    action <- eins [ some_constant , some_operation ]
    action

some_constant = do
    i <- eins [ 0 .. 3 ]
    c <- eins [ 0 .. 1 ]
    return $ Constant i c

some_operation = do
    i <- eins [ 0 .. 3 ]
    j <- eins [ 0 .. 3 ]
    k <- eins [ 0 .. 3 ]    
    op <- eins [ Add, Mul ]
    return $ op i j k

change_program [] = return [ Constant 0 1 ]
change_program p = do
    i <- randomRIO ( 0, length p - 1 )
    let ( pre , this : post ) = splitAt i p
    that <- change this
    return $ pre ++ that : post

change s = case s of
    Constant i c -> do
        j <- randomRIO ( 0, i+1 )
	d <- randomRIO ( 0, c+1 )
	return $ Constant j d
    Add i j k -> change_operation i j k
    Mul i j k -> change_operation i j k

change_operation i j k = do
    ii <- randomRIO ( max 0 $ i-1, i+1 )    
    jj <- randomRIO ( max 0 $ j-1, j+1 )    
    kk <- randomRIO ( max 0 $ k-1, k+1 )    
    op <- eins [ Mul, Add ]
    return $ op ii jj kk

-- | costs on the Smallnums 1 model
cost :: Statement -> Int
cost s = case s of
    Constant i c -> patch i + patch c + 1
    Add i j k  -> 4 + sum [ patch i, patch j, patch k ]
    Mul i j k  -> 4 + sum [ patch i, patch j, patch k ]

patch i = max 1 $ 2*i-1

-- | the value that is left in x0 finally
value :: Program -> Integer
value stmts = 
    let fm = foldl execute emptyFM stmts
    in  access fm 0

access fm i = case lookupFM fm i of
    Just x -> x
    Nothing -> 0

execute state action = case action of
    Constant i c -> addToFM state i $ fromIntegral c
    Add i j k -> operation state (+) i j k
    Mul i j k -> operation state (*) i j k
    
operation state op i j k = 
    let x = access state j
	y = access state k
    in  addToFM state i $ op x y



-- local variables:
-- mode: haskell
-- end:
