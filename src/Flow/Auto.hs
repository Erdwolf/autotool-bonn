module Flow.Auto where

import Autolib.ENFA
import Autolib.ENFA.Uneps
import qualified Autolib.Relation

import Autolib.ToDoc

eps_builder_all_final 
    :: NFAC c s 
    => [s] -> [ (s, Maybe c, s) ] -> ENFA c s
eps_builder_all_final is ts = 
    let ss = mkSet $ is ++ do ( p, mc, q ) <- ts ; [ p, q ]
    in  ENFA
        { enfa_info = text "eps_builder"
        , alphabet = mkSet $ do ( p, Just c, q ) <- ts ; return c
        , states = ss
        , starts = mkSet is
        , finals = ss
        , trans = tcollect 
             $ do ( p, Just c, q ) <- ts ; return ( p, c, q )
        , mirror_trans = tcollect 
             $ do ( p, Just c, q ) <- ts ; return ( q, c, p )
        , eps = Autolib.Relation.reflex_trans
	      $ Autolib.Relation.make_on (ss, ss )
	      $ do ( p, Nothing , q ) <- ts ; return ( p, q )
        , mirror_eps = Autolib.Relation.reflex_trans
	      $ Autolib.Relation.make_on (ss, ss )
	      $ do ( p, Nothing , q ) <- ts ; return ( q, p )
	, eps_is_trans_reflex = True
	}

