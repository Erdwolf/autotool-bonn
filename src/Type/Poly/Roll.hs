module Type.Poly.Roll where

import Type.Poly.Data

import Autolib.Util.Zufall
import Autolib.TES.Identifier
import Autolib.Util.Size

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad ( forM, guard ) 
import Data.List ( nub , minimumBy)
import Data.Maybe (isJust)
import Data.Ord ( comparing )
import Data.Ix ( inRange )

roller conf = do
    let handle k = 
            if k <= 0 
            then error "Type.Poly.Roll: cannot generate problem instance"
            else do
                txss <- roll conf
                case concat txss of
                     [] -> handle (k-1)
                     txs  -> return $ minimumBy 
                                 ( comparing $ size . target . fst ) 
                                 txs
    handle $ generator_retries conf 

roll conf = forM [ 1 .. generator_iterations conf ] $ \ i -> do
    sig <- Type.Poly.Roll.signature conf
    let (lo, hi) = solution_size_range conf
        candidates = concat $ take hi $ typed_terms sig
        census = M.fromListWith min $ do
            (t,x) <- candidates 
            return ( t
                   , ( size x
                     , ( TI { target = t , Type.Poly.Data.signature = sig } , x )
                     )
                   )  
    let allfun = S.fromList $ function_names conf
    return $ do     
            (s, out) <- M.elems census
            guard $ inRange ( lo,hi ) s
            guard $ S.isSubsetOf allfun $ expression_signature ( snd out ) 
            return out
                    
-- | lazy infinite list,
-- on index k: terms of size k (with their type)
typed_terms :: Signature -> [[ ( Type, Expression ) ]]
typed_terms sig = output where 
  output = do 
    total <- [ 0 .. ]
    return $ do
        f <- Type.Poly.Data.functions sig
        parts <- 
            distributions1 (total - 1) (length $ arguments f)
        ( sub, args )  <- 
            matches $ zip ( arguments f )
                  $ map ( \ p -> output !! p ) parts
        let sub_in_order = do
                v <- tyvars f
                return $ sub M.! v
        return ( apply sub $ result f 
               , Apply sub_in_order ( fname f ) args
               )

distributions1 total slots | total <= 0 || slots <= 0 = do
    guard $ total == 0 && slots == 0
    return [ ]
distributions1 total 1 = return [ total ]
distributions1 total slots = do
    x <- [ 1 .. total - slots + 1 ] 
    xs <- distributions1 ( total - x ) ( slots - 1 )
    return $ x : xs

matches :: [(Type, [(Type, Expression)])]
      -> [ (M.Map Identifier Type, [Expression]) ]
matches [] = return ( M.empty, [] )         
matches (( s, txs) : rest) = do
    (t,x) <- txs
    m <- match s t
    let rest' = map ( \ (s,tys) -> ( apply m s, tys)) rest
    ( m', xs ) <- matches rest'    
    return ( M.unionWith (error "matches") m m'
           , x : xs  
           )  
    
-- | variables in left arg are bound.    
-- second arg shall not have variables    
match :: Type -> Type -> [ M.Map Identifier Type ]    
match s t = case (s, t) of
    (TyVar v, t) -> return $ M.fromList [ (v, t) ]
    (TyCon f xs, TyCon g ys) | f == g -> match_list xs ys
    _ -> []
    
match_list [] [] = return M.empty    
match_list (x:xs) (y:ys) = do
    m <- match x y
    m' <- match_list ( map ( apply m ) xs ) ys
    return $ M.unionWith 
      (error $ "match_list: " ++ show ((x:xs, y:ys), (m, m') )) m m'      

apply :: M.Map Identifier Type -> Type -> Type
apply m t = case t of
    TyVar v -> case M.lookup v m of
        Nothing -> t
        Just s  -> s
    TyCon f args ->    
        TyCon f ( map ( apply m ) args )

-------------------------------------------------------- 
      
signature :: Conf -> IO Signature
signature conf = do
    fs <- Type.Poly.Roll.functions conf
    return $ Signature { Type.Poly.Data.functions = fs }

functions :: Conf -> IO [ Function ]
functions conf = forM ( function_names conf ) $ \ n -> do
    a <- randomRIO $ arity_range conf
    res : args <- forM [ 0, 1 .. a ] $ \ k -> do
        s <- randomRIO $ type_expression_size_range conf
        type_scheme conf s
    let vars_in_args = nub $ do 
          t <- args ; TyVar v <- subtypes t ; return v
    let nullaries = do 
            ( t, 0) <- types_with_arities conf ; return t
        repair t = case t of               
            TyVar v -> 
                  if v `elem` vars_in_args
                  then return t
                  else do 
                      f <- eins nullaries
                      return $ TyCon f []
            TyCon f args -> do
                xs <- forM args repair
                return $ TyCon f xs
    res' <- repair res            
    return $ Function { fname = n
              , tyvars = vars_in_args
	      , arguments = args
	      , result = res'
	      }

type_scheme :: Conf
               -> Int -- ^ size
                  -> IO ( Type )
type_scheme conf total | total >= 0 = do
    let couldbe = 
          filter ( \ (t,a) -> ( total > a )
                           && ( a > 0 || total == 1 ) ) 
                $ types_with_arities conf
        mkvar =   do 
           v <- eins $ type_variables conf
           return $ TyVar v
    if null couldbe 
       then mkvar
       else do 
           (t,a) <- eins couldbe
           var_flag <- if a > 0 then return False
                       else randomRIO ( False, True ) 
           if var_flag 
               then mkvar 
               else do     
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
    