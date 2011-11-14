module Program.ArrayBonn.Roll where

import Program.ArrayBonn.Value
import Program.ArrayBonn.Statement
import Program.ArrayBonn.Semantics
import Program.ArrayBonn.Expression
import Program.ArrayBonn.Statement
import Program.ArrayBonn.Operator

import qualified Program.GeneralBonn.Environment as E
import qualified Program.GeneralBonn.Program as P


import qualified Program.ArrayBonn.Config as C
import qualified Program.ArrayBonn.Value as V

import Autolib.Util.Zufall
import Autolib.TES.Identifier
import Autolib.Reporter

import Data.Ix

type Environment = E.Environment Value
type Program = P.Program Statement

roll :: RandomC m
     => C.Config
     -> m ( Environment , Program, Environment )
roll c = do
    env0 <- rollenv c
    ( sts, env1 ) <- statements c env0
    return ( env0, P.Program sts, env1 )
  `repeat_until` \ ( e0, p, e1 ) -> e0 /= e1

statements :: RandomC m
	   => C.Config
	   -> Environment
	   -> m ( [Statement], Environment )
statements c env0 = 
    if C.statements c <= 0 
    then return ( [], env0 )
    else do
        let s = C.max_data_size c
	    t = round $ sqrt $ fromIntegral s
        ( st, env1 ) <- 
	    rollstat env0 ( C.operators c ) ( 0, t ) ( C.max_expression_size c )
	( sts, env2 ) <- 
	    statements ( c { C.statements = C.statements c - 1 } ) env1
	return ( st : sts, env2 )

-- | from given environment, roll statement and present resulting environment
rollstat :: RandomC m
     => Environment    
     -> [ Operator ]
     -> ( Integer, Integer )
     -> Int -- ^  size
     -> m ( Statement, Environment )
rollstat env ops bnd s = do
    ( acc @ ( Access name inds ) , vs ) <- rollacc env ops bnd s
    ( exp, val ) <- rollex env ops bnd s
    let st = Assign acc exp
	menv' = result $ Program.ArrayBonn.Semantics.single env st
    case menv' of
	Just env' | env /= env' -> return ( st, env' )
        _ -> rollstat env ops bnd s

rollenv :: RandomC m
	=> C.Config
	-> m Environment
rollenv c = do
    let vds = zip [ 'a' .. ] $ do
	        (d, n) <- zip [ 0.. ] $ C.variables_per_depth c
		replicate n d
    pairs <- sequence $ do
        (v, d) <- vds
	return $ do
	    let name = mkunary [v]
	    val <- V.roll d $ C.max_data_size c
	    return ( name, val )
    return $ E.make pairs

-- | roll expression with value in given range.
-- it is important to adhere to the range
-- since expression will be used for array indexing
rollex :: RandomC m
     => Environment    
     -> [ Operator ]
     -> ( Integer, Integer )
     -> Int -- ^  size
     -> m ( Expression, Integer )
rollex env ops bnd s0 = do
    s <- randomRIO ( max 1 $ s0 `div` 2, s0 )
    let binary = do
            sl <- randomRIO ( 1, s - 1 )
	    let sr = s - sl
	    x <- rollex env ops bnd sl
	    y <- reference sr
	    [ (l,vl), (r,vr) ] <- eins [ [x,y],[y,x] ] 
	    op <- eins ops
	    return ( Binary op l r, semantics op vl vr )
        reference s = do
	    ( acc @ ( Access name inds ) , vs ) <- rollacc env ops bnd s	
	    let Just val = E.lookup env name
	    return ( Reference acc , V.get val vs )
    if s <= 2
        then literal bnd
        else repeat_until_X 20
	     ( do action <- eins [ binary , reference $ s - 1 ] ; action )
             ( \ ( x, v ) -> not ( trivial x ) && inRange bnd v )
	     ( literal bnd )
        
repeat_until_X num action check fallback = do
    x <- action
    if check x 
       then return x
       else if num < 0 then fallback
	    else repeat_until_X ( num - 1 ) action check fallback

literal bnd = do 
    i <- randomRIO bnd 
    return ( Literal i, i )


trivial x = case x of
    Binary Add ( Literal 0 ) _ -> True
    Binary Add _ ( Literal 0 ) -> True
    Binary Subtract _ ( Literal 0 ) -> True
    Binary Multiply ( Literal 0 ) _ -> True
    Binary Multiply _ ( Literal 0 ) -> True
    Binary Multiply ( Literal 1 ) _ -> True
    Binary Multiply _ ( Literal 1 ) -> True
    Binary Divide _ ( Literal 1 ) -> True
    Binary Divide _ ( Literal 0 ) -> True
    _ -> False

rollacc env ops bnd s = do
    ( p, v ) <- eins $ E.contents env
    let ds = V.depth v
	sub = s `div` length ds
    xvs <- mapM ( \ d -> rollex env ops ( 0, fromIntegral d - 1 ) sub ) ds
    return ( Access p $ map fst xvs , map snd xvs )

