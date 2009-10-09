{-# LANGUAGE TemplateHaskell #-}

module Types.TaskTree (
    TaskTree (..)
) where

import Types.Basic

import Data.Autolib.Transport

data TaskTree = Task Task
              | Category Name [TaskTree]

$(derives [makeToTransport] [''TaskTree])
