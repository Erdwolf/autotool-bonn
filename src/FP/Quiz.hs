{-# OPTIONS -fglasgow-exts #-}

module FP.Quiz where

import FP.Check
import FP.Conf
import FP.Instance
import FP.Roll
import FP.Expression

import Autolib.Size
import Autolib.ToDoc

import Autolib.TES.Identifier

import Control.Monad ( guard )

import Inter.Quiz
import Inter.Types

import Debug
import Data.Array


instance Generator FPTypeCheck Conf ( Expression Identifier, TI ) where
    generator p conf key = nice_problem conf


instance Project  FPTypeCheck ( Expression Identifier, TI ) TI where
    project p ( x , ti ) = ti

make :: Make
make = quiz FPTypeCheck FP.Conf.example


