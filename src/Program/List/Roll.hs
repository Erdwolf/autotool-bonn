module Program.List.Roll where

import qualified Program.List.Expression as X
import qualified Program.List.Semantics as S
import qualified Program.List.Operation as O
import qualified Program.List.Value as V
import qualified Program.General.Environment as E
import qualified Program.General.Program as P
import qualified Program.List.Config as C

import Autolib.TES.Identifier
import Autolib.Util.Zufall
import Control.Monad ( forM )
import Data.Maybe ( isJust )
import Autolib.Reporter ( result)

environment ::C.Config 
            -> IO ( E.Environment V.Value )
environment conf = do
    binds <- forM ( C.variables conf ) $ \ ( C.Declaration ty na ) -> do
        d <- datah ty $ C.data_size_bounds conf
        return ( na, d )
    return $ E.make binds

datah :: V.Type
          -> (Int,Int) -- ^ data size range
          -> IO V.Value
datah ( ty @ V.TCollect { V.name =  n, V.arg = V.TScalar } ) (lo,hi) = do
    s <- randomRIO (lo,hi)
    xs <- forM [ 1 .. s ] $ \ _ -> do
            randomRIO (0, fromIntegral lo )
    return $ V.Collect 
           { V.typeof = ty
           , V.contents = map V.Scalar xs
           }

program ::  [ O.Operation ]
           -> E.Environment V.Value
           -> C.Config
           -> IO ( P.Program S.Statement )
program ops env conf = do
    let handle e k | k > 0 = do
            d <- randomRIO $ C.expression_depth_bounds conf
            (x, f) <- statement ops e d
            xs <- handle f (k-1)
            return $ x : xs
        handle e 0 = return []
    l <- randomRIO $ C.program_length_bounds conf
    xs <- handle env l
    return $ P.Program xs

statement ::  [ O.Operation ]
           -> E.Environment V.Value
           -> Int -- ^ max. nesting depth
           -> IO (S.Statement, E.Environment V.Value)
statement ops env d = do
    (s, Just f) <- do   x <- expression True ops env O.Void d
                        return ( S.Statement x, result $ S.execute env $ P.Program [ S.Statement x ] ) 
          `repeat_until` \ (x, mf) -> isJust mf
    return (s, f )

expression :: Bool -- ^ must be toplevel (statement)?
           -> [ O.Operation ]
           -> E.Environment V.Value
           -> O.Type -- ^ result type
           -> Int  -- ^ max. nesting depth of expression
           -> IO X.Expression
expression top ops env ty d | d > 0 = do
    ( name, val ) <- eins 
          $ filter ( \ (name, val ) -> case val of
                          V.Collect {} -> True
                          _ -> False
                   )
          $ E.contents env
    case    filter ( \ op -> O.object op == V.name ( V.typeof val ) )
          -- $ filter ( \ op -> O.result op `conform` ty )
          $ filter ( \ op -> O.toplevel op == top )
          $ ops  of
        [] -> do
            -- expression top ops env ty 0 
            return $ X.Scalar $ fromIntegral d
        ops -> do
            op <- eins ops         
            xs <- forM ( O.args op ) $ \ arg -> do
                d <- randomRIO ( 0, d-1)
                expression False ops env arg d
            return $ X.Methodcall ( X.Reference name ) ( mkunary $ O.method op ) xs
    
expression top ops env ty d | d <= 0 = do
    i <- randomRIO ( 0, 3 )
    return $ X.Scalar i

t1 `conform` t2 = case (t1, t2) of
    -- (O.Void, O.Void) -> True
    (_, O.Void) -> True
    (O.Void, _) -> False
    (_, _) -> True -- what?

