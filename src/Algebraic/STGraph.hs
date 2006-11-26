{-# OPTIONS -fglasgow-exts #-}

module Algebraic.STGraph where

import qualified Autolib.TES.Binu as B

import Expression.Op
import Graph.Series_Parallel.Type
import qualified Graph.Series_Parallel.Op  as SPO

import Graph.Iso
import Graph.Util

import Condition

import Algebraic.Class
import Algebraic.Instance
import Data.Typeable

import Autolib.Dot ( peng, Layout_Program (..) )
import Autolib.Graph.Graph

import Autolib.ToDoc
import Autolib.Choose
import Autolib.Reader
import Autolib.Set

instance Ops ( STGraph Int ) where
   bops = B.Binu
        { B.nullary = 
              [ Op { name = "v", arity = 0
                   , precedence = Nothing
                   , assoc = AssocNone
                   , inter = \ [ ] -> return $ SPO.point
                   }
              , Op { name = "e", arity = 0
                   , precedence = Nothing
                   , assoc = AssocNone
                   , inter = \ [ ] -> return $ SPO.edge
                   }
              ]
        , B.unary = []
        , B.binary = 
              [ Op { name = "+", arity = 2
                   , precedence = Just 7, assoc = AssocLeft
                   , inter = \ [ x, y ] -> do
                        return $ SPO.normalize 
                               $ SPO.parallel x y
                   }
              , Op { name = "*", arity = 2
                   , precedence = Just 8, assoc = AssocLeft
                   , inter = \ [ x, y ] -> do
                        return $ SPO.normalize
                               $ SPO.serial x y
                   }
              ]
        }

data Algebraic_STGraph = Algebraic_STGraph 
    deriving ( Read, Show, Typeable )

instance Condition () ( STGraph a ) where 
    explain p = toDoc ()
    condition p g = return ()

instance Algebraic Algebraic_STGraph ( STGraph Int ) where
    -- evaluate         :: tag -> Exp a -> Reporter a
    evaluate tag exp = do
        g <- tfoldR ( \ it -> reject $ text "unbekannt:" <+> toDoc it
                    ) inter exp
        let h = contents g
	inform $ vcat 
               [ text "der zugrundeliegende Graph ist" 
               , nest 4 $ toDoc h
               ]
        ping h
	return g

    present tag g = do
	ping $ contents g
        
    -- equivalent       :: tag -> a -> a -> Reporter Bool
    equivalent tag a b = do
       return $ iso ( contents a ) ( contents b )

    -- some_formula     :: tag -> Algebraic.Instance.Type a -> Exp a
    some_formula tag i = read "(e * e) + e"

    -- default_instance :: tag -> Algebraic.Instance.Type a
    default_instance tag = Algebraic.Instance.Make
        { Algebraic.Instance.target 
              = Graph.Series_Parallel.Type.example
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
