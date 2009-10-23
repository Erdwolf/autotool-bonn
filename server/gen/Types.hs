module Types (
    AData (..),
    ACons (..),
    AType (..),
    consName
) where

data AData
    = AData AType [ACons]
    deriving Show

data ACons
    = ACons String [AType]
    | ARec String [(String, AType)]
    deriving Show

data AType
    = AType String [AType]
    | AVar String
    deriving Show

consName :: ACons -> String
consName (ACons nm _) = nm
consName (ARec nm _) = nm
