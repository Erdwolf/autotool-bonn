{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, DeriveDataTypeable, TemplateHaskell #-}
module AVLBaumBonn.Quiz where

import Inter.Quiz
import Inter.Types

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

import qualified Baum.Such.Config
import qualified Baum.Such.Generate

import AVLBaumBonn.Central



data QuizConfig = QuizConfig
	    { start_size :: Int
        , min_key :: Int
	    , max_key :: Int
	    , fixed_insert_ops :: Int
	    , guess_insert_ops :: Int
	    }
     deriving Typeable

$(derives [makeReader, makeToDoc] [''QuizConfig])

instance Generator AVLBaum QuizConfig Config where
    generator _ (QuizConfig ss l h f g) _key =
        Baum.Such.Generate.generate $ Baum.Such.Config.Config ss l h f 0 g 0

instance Project AVLBaum Config Config where
    project _ i = i

make :: Make
make = quiz AVLBaum $ QuizConfig { start_size = 10
                                 , min_key    = 1
                                 , max_key    = 99
                                 , fixed_ops  = 5
                                 , guess_ops  = 5
                                 }
