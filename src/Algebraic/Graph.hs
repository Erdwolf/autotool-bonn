{-# OPTIONS -fglasgow-exts #-}

module Algebraic.Graph where

import qualified Autolib.TES.Binu as B

import Graph.Op
import Graph.Iso
import Graph.Util
import Graph.Restriction
import Autolib.Graph.Kneser ( petersen )

import Algebraic.Class
import Algebraic.Instance
import Data.Typeable

import Autolib.Dot ( peng, Layout_Program (..) )
import Autolib.Graph.Graph

import Autolib.ToDoc
import Autolib.Choose
import Autolib.Reader
import Autolib.Set

data Algebraic_Graph = Algebraic_Graph deriving ( Read, Show, Typeable )

instance Algebraic Algebraic_Graph ( Graph Int ) where
    -- evaluate         :: tag -> Exp a -> Reporter a
    evaluate tag exp = do
        g <- tfoldR ( \ it -> reject $ text "unbekannt:" <+> toDoc it
                    ) inter exp
	inform $ vcat [ text "Graph ist" , nest 4 $ toDoc g ]
        ping g
	return g

    present tag g = do
	ping g
        
    -- equivalent       :: tag -> a -> a -> Reporter Bool
    equivalent tag a b = do
       return $ iso a b

    -- some_formula     :: tag -> Algebraic.Instance.Type a -> Exp a
    some_formula tag i = Graph.Op.example

    -- default_instance :: tag -> Algebraic.Instance.Type a
    default_instance tag = Algebraic.Instance.Make
        { target = petersen
          , description = Nothing
	  , operators = default_operators tag
          , max_size = 7
	}


    default_operators tag = bops


ping g = peng $ ( no_fixed_layout g )
	     { layout_program = Fdp
             , layout_hints = [ "-Nheight=0.1", "-Nwidth=0.1"
                              , "-Nfixedsize=true"
                              , "-Gsize=7,7"
                              ]
	     , show_labels = False
             } 