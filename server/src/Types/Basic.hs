module Types.Basic (
    Name, Task, Seed, Signature
) where

import Util.Hash

-- basic types
type Name = String
type Task = String
type Seed = String
type Signature = Digest
