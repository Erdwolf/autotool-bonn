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

import qualified Baum.Such.Config
import qualified Baum.Such.Generate

import AVLBaumBonn.Central



data QuizConfig = QuizConfig
	    { start_size :: Int
        , min_key    :: Int
	    , max_key    :: Int
	    , fixed_ops  :: Int
	    , guess_ops  :: Int
	    , quiz_feedback :: Feedback
	    }
     deriving Typeable

$(derives [makeReader, makeToDoc] [''QuizConfig])

instance Generator AVLBaum QuizConfig Config where
    generator _ (QuizConfig ss l h f g) _key =
        let loop = do
              cfg@(_,_,t) <- Baum.Such.Generate.generate_once $ Baum.Such.Config.Config ss l h f 0 g 0
              if containsDuplicateElements t
                 then loop
                 else return cfg
        in loop

containsDuplicateElements =
    not . null . dup . elements

elements = catMaybes . flatten . toTree

dup xs = xs \\ nub xs


instance Project AVLBaum Config Config where
    project _ i = i

make :: Make
make = quiz AVLBaum $ QuizConfig { start_size    = 10
                                 , min_key       = 1
                                 , max_key       = 50
                                 , fixed_ops     = 5
                                 , guess_ops     = 5
                                 , quiz_feedback = Always
                                 }
