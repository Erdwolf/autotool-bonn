module Hilbert.Look

( search
)

where

import Autolib.Set
import Autolib.Util.Sort
import Autolib.Util.Hide
import Autolib.FiniteMap
import Autolib.Util.Uniq

import Hilbert.Infer
import Hilbert.Env
import Hilbert.Axioms
import Hilbert.Sat

import Autolib.TES.Term hiding ( unvar, assoc, precedence, arity )
import Autolib.Size

import Autolib.Schichten
import Hilbert.BFS
import Hilbert.Proof

import Autolib.TES.Position
import Boolean.Op
import Expression.Op
import Autolib.ToDoc

import Data.Tree

import Control.Monad ( guard, when )
import System.IO

maxdepth = 15	-- of tree
maxbranch = 500	-- of number of children of a tree node
maxwidth = 4	-- of clauses in search target

maxsize = 20	-- of formulas in target
maxvars = 4	-- of variables in target

maxsols = 10	-- number of solutions


search :: Exp Bool -> IO ()
search goal = search' 0 $ unvar goal

search' dep goal = 
    do putStrLn $ "**** at depth " ++ show dep
       paths <- lookfor_bfs ( syms goal ) [ goal ] dep [ ] 
       if null paths
	  then search' (dep + 1) goal
	  else return ()

----------------------------------------------------------------       

lookfor_bfs alphabet targets top path = do
  sequence_ $ do
    ( n, Hide inf ) <- Hilbert.BFS.weighted_search 
      ( \ ( ts, Hide p ) -> 
              ( not $ null ts
	      ,  0 
	       + ( sum $ map ( size ) ts )
	      -- , ( length ts ) 
	      ) 
      )
      ( \ ( ts, Hide p ) ->  mkSet $ do
        let alphabet' = alphabet `union` mkSet [ nicht ]
	( n, Hide i ) <- 
	    infer ( usable_rules alphabet' axioms ) ts
	guard $ not $ subseteq ( mkSet targets ) ( mkSet n )
	guard $ all ( not . isvar ) n
        guard $ satisfiable n
	guard $ klein top n
	return ( n , Hide ( i : p ) )
      ) 
      ( targets, Hide path ) 
    return $ do 
        when ( length n <= 1 ) $ do
	    hPutStrLn stderr $ show $ toDoc ( n {-- , inf --} ) 
	    hPutStrLn stderr "--------------------------"
	hPutStr stderr $ show $ length n 
	when ( null n ) $ do
	    putStrLn "\n--------------------------"
--	    putStrLn $ show ( toDoc $ treeform $ explain inf )
	    putStrLn "\n--------------------------"
	    putStrLn $ show ( toDoc $ zip [1 :: Int ..] $ explain inf )
	    hFlush stdout
	    error "-----------------------------------"
  return []


klein top ts = and
      [ True
--      , length ts < top
      , sum ( map size ts ) < top
      , sum ( map ( length . varpos ) ts ) < 4
--    , cardinality ( unionManySets $ map vars ts ) < top
      ]

usable_rules alphabet axioms = do
    ( name, form ) <- contents axioms
    guard $ subseteq ( syms form  ) ( alphabet `union` mkSet [ implies ] )
    ( prems, conc ) <- distributes form
    let pvars = unionManySets $ map vars prems
    return ( prems, conc )

----------------------------------------------------------------       

treeform steps = 
    let collect accu [] = accu
	collect accu ( (t, p) : odo ) = 
	    let p' = resolve accu p
	        accu' = addListToFM_C ( error "treeform.collect" ) 
				      accu [ (t, p') ]
	    in collect accu' odo
        index = collect emptyFM steps
    in  lookupWithDefaultFM index ( error $ "treeform.lookup:" ) 
	    $ fst $ last steps

externalize :: Proof -> Tree String
externalize p = case p of
    Modus_Ponens {} -> 
        Data.Tree.Node "Mopo" $ map externalize [ left p, right p ]
    

resolve index p = case p of
    Reference {} -> 
        lookupWithDefaultFM index ( error "treeform.resolve" ) $ core p
    Modus_Ponens {} -> 
        Modus_Ponens { left = resolve index $ left p 
		     , right = resolve index $ right p
		     }
    Local_Substitution { to = Axiom {} } ->
        p

explain [] = []
explain ( ( f, p ) : fps ) = case p of
    Global_Substitution {} -> 
        map ( applyP ( fm p ) ) $ explain ( (f, to p) : fps )
    _ -> ( f, p ) : explain fps
        
applyP fm ( f, p ) = ( apply_partial fm f, apply_to_proof fm p )

apply_to_proof subst p = case p of
     Reference {} -> Reference { core = apply_partial subst $ core p }
     Modus_Ponens {} -> 
         Modus_Ponens { left  = apply_to_proof subst $ left p
		      , right = apply_to_proof  subst $ right p 
		      }
     Local_Substitution {} ->
         apply_to_proof ( apply_to_sub subst $ fm p ) $ to p
     Axiom {} -> 
	 Local_Substitution { fm = subst , to = p }

apply_to_sub fm sub = mapFM ( \ k v -> apply_partial fm v ) sub

------------------------------------------------------------------


