module Robots2.Examples where

-- -- $Id$

import Robots2.Data
import Robots2.Config
import Robots2.Nice

import Autolib.ToDoc
import Autolib.Set

-- | Aufgabe von Alexander Krabbes
-- (für die Original-Spielregeln)
alex :: Config 
alex = make
  [ (-4,5), (4,5), (3,4), (-3,4), (0,5), (4,-4), (-4,-4) ] 
  [ (0,0) ] 

