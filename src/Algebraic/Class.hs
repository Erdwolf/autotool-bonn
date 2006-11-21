{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-incoherent-instances #-}

module Algebraic.Class 

( Algebraic (..)
, module Expression.Op
, module Autolib.Reporter
, module Autolib.ToDoc
)

where

import Algebraic.Instance

import Expression.Op

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

class (        Reader tag, ToDoc tag, Typeable tag
      , Show tag, Read tag
      , Ops a, Reader a  , ToDoc a  , Typeable a
      ) => Algebraic tag a | tag -> a where
    evaluate         :: tag -> Exp a -> Reporter a
    equivalent       :: tag -> a -> a -> Reporter Bool
    some_formula     :: tag -> Algebraic.Instance.Type a -> Exp a
    default_instance :: tag -> Algebraic.Instance.Type a
