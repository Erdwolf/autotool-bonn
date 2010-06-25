-- Simplified type information for Haskell modules.

module Types (
    AData (..),
    ACons (..),
    AType (..),
    consName
) where

-- a data type: a type and a list of alternative constructors
data AData
    = AData AType [ACons]
    deriving Show

-- a data constructor
data ACons
    = ACons String [AType]          -- simple constructor with anonymous fields
    | ARec String [(String, AType)] -- record constructor with named fields
    deriving Show

-- a type: a type constructor with type arguments, or a type variable
data AType
    = AType String [AType]
    | AVar String
    deriving Show

-- return the constructor name of a constructor
consName :: ACons -> String
consName (ACons nm _) = nm
consName (ARec nm _) = nm
