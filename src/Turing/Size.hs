module Turing.Size where

-- $Id$

import Size


instance Size (Turing y z) where
    size = size . tafel


