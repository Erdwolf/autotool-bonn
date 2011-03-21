module Type.Poly.Roll where

import Type.Poly.Data

import Autolib.Util.Zufall
import Autolib.TES.Identifier

import qualified Data.Map as M
import Control.Monad ( forM ) 
import Data.List ( nub )

{-
data Conf = Conf { types_with_arities :: [ (Identifier, Int) ]
                 , type_variables :: [ Identifier ]
                 , function_names :: [ Identifier ]
                 , type_expression_size_range :: (Int,Int)  
                 , arity_range :: (Int, Int) -- ^ min arity should be zero
                 , solution_size_range :: (Int,Int)  
		 }
-}

functions :: Conf -> IO [ Function ]
functions conf = forM ( function_names conf ) $ \ n -> do
    a <- randomRIO $ arity_range conf
    ts @ ( res : args ) <- forM [ 0, 1 .. a ] $ \ k -> do
        s <- randomRIO $ type_expression_size_range conf
        type_scheme conf s
    let vars = nub $ do t <- ts ; TyVar v <- subtypes t ; return v
    return $ Function { fname = n
              , tyvars = vars
	      , arguments = args
	      , result = res
	      }

type_scheme :: Conf
               -> Int -- ^ size
                  -> IO ( Type )
type_scheme conf total | total >= 0 = do
    let couldbe = filter ( \ (t,a) -> total > a && ( a > 0 || total == 1 ) ) 
                $ types_with_arities conf
    if null couldbe 
       then do 
           v <- eins $ type_variables conf
           return $ TyVar v
       else do 
           (t,a) <- eins couldbe
           subs <- distribute1 (total - 1) a
           ts <- forM subs $ type_scheme conf
           return $ TyCon t ts

-- | sum is total, each number is >= 0
distribute0 :: Int -> Int -> IO [ Int ]
distribute0 total slots | total >= 0 = do
    xs <- forM [ 1 .. total ] $ \ k -> randomRIO ( 1, slots )
    let m = M.fromListWith (+) $ zip xs $ repeat 1
    return $ do k <- [ 1 .. slots ] ; return $ M.findWithDefault 0 k m
    
-- | PRE: sum >= slots, sum is total, each number is >= 1    
distribute1 :: Int -> Int -> IO [ Int ]    
distribute1 total slots = do
    xs <- distribute0 (total - slots) slots
    return $ map succ xs
    