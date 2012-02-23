{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, DeriveDataTypeable, TemplateHaskell #-}
module AVLBaumBonn.Quiz where

import Inter.Quiz
import Inter.Types

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

import Data.Tree (flatten)
import Data.List ((\\), nub)
import Data.Maybe (catMaybes)
import Control.Monad (liftM)

import qualified Baum.Such.Config
import qualified Baum.Such.Generate
import Baum.AVL.Type (key, left, right, leaf)

import AVLBaumBonn.Central



data QuizConfig = QuizConfig
	    { quiz_feedback :: Feedback
	    , start_size :: Int
        , min_key    :: Int
	    , max_key    :: Int
	    , fixed_ops  :: Int
	    , guess_ops  :: Int
	    , discard_prefix_trees :: Bool
	    }
     deriving Typeable

$(derives [makeReader, makeToDoc] [''QuizConfig])

instance Generator AVLBaum QuizConfig Config where
    generator _ (QuizConfig fb ss l h f g p) _key = do
        let loop = do
              cfg@(s,_,t) <- Baum.Such.Generate.generate_once $ Baum.Such.Config.Config ss l h f 0 g 0
              if p && isPrefix s t || containsDuplicateElements t
                 then loop
                 else return cfg
        (t1,os,t2) <- loop
        return (Config fb (bonnifyTree t1) os (bonnifyTree t2))

containsDuplicateElements =
    not . null . dup . elements

elements = catMaybes . flatten . toTree

dup xs = xs \\ nub xs

isPrefix t _ | leaf t = True
isPrefix t1 t2 | key t1  == t2 = isPrefix (left t1) (left t2) && isPrefix (right t1) (right t2)
isPrefix _ _ = False

instance Project AVLBaum Config Config where
    project _ i = i

make :: Make
make = quiz AVLBaum $ QuizConfig { quiz_feedback = Always
                                 , start_size    = 10
                                 , min_key       = 1
                                 , max_key       = 50
                                 , fixed_ops     = 5
                                 , guess_ops     = 5
                                 , discard_prefix_trees = True
                                 }
