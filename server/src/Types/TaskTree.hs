{-# LANGUAGE TemplateHaskell #-}

module Types.TaskTree (
    TaskTree (..)
) where

import Types.Basic

import Data.Autolib.Transport

data TaskTree = Task     { task_name :: Task }
              | Category { category_name :: Name,
                           sub_trees :: [TaskTree] }
    deriving (Read, Show)

$(derives [makeToTransport] [''TaskTree])
