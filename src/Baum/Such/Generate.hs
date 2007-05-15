module Baum.Such.Generate where

--  $Id$

import Baum.Such.Class
import Baum.Such.Op
import Baum.Such.Config

import Autolib.Set
import Autolib.Util.Zufall


type Instanz baum a = ( baum a, [ Op a ], baum a )

generate :: ( Random a, Such baum, OpC a )
	 => Config a
	 -> IO ( Instanz baum a )
generate conf = do
    let key = randomRIO (min_key conf, max_key conf)
    keys <- sequence $ replicate ( start_size conf ) key
    let start = foldl insert empty keys
    
    let inserts = replicate (fixed_insert_ops conf) (  True,  True )
	       ++ replicate (guess_insert_ops conf) (  True, False )
        deletes = replicate (fixed_delete_ops conf) ( False,  True )
	       ++ replicate (guess_delete_ops conf) ( False, False )
    ins <- permutation inserts
    dels <- permutation deletes
    codes <- permutation $ ins ++ dels

    let gen b [] = return ([], b)
        gen b ( (t, v) : tvs) = do
	    a <- if t then key else eins $ contents b
	    let op = if v then ( if t then Insert a else Delete a )
		          else Any
                c = ( if t then insert else delete ) b a
	    (ops, d) <- gen c tvs
            return ( op : ops, d )
    ( ops, end ) <- gen start codes

    return ( start, ops, end )
