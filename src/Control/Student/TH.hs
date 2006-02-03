{-# OPTIONS -fallow-overlapping-instances -fth #-}

module Control.Student.TH where

import Control.Student.Type
import Inter.TH
import Control.TH
import Network.XmlRpc.Server

$(Inter.TH.helper ''Student)
