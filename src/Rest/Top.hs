{-# LANGUAGE ScopedTypeVariables #-}

module Rest.Top where

import Happstack.Server.SimpleHTTP
import Control.Monad ( msum, liftM )

import qualified Inter.Collector as IC

----------------------------------------------------

data Task = Task String deriving Show
instance FromData Task where
    fromData = liftM Task $ look "task" 

data Config = Config String deriving Show
instance FromData Config where
    fromData = liftM Config $ look "config" 

data Instance = Instance String deriving Show
instance FromData Instance where
    fromData = liftM Instance $ look "instance" 

data Solution = Solution String deriving Show
instance FromData Solution where
    fromData = liftM Solution $ look "solution" 

---------------------------------------------------

{- for testing, use curl:

curl -X POST http://localhost:9876/GradeTaskSolution -F task=foo -F instance=bar -F solution=foo

-}

main = simpleHTTP 
           ( Conf { port = 9876, validator = Nothing } )
     $ msum 
    [ dir "GetServerInfo" $ do
        return $ toResponse "I am your server."
    , dir "GetTaskTypes" $ do
        return $ toResponse $ unlines $ do
            m <- IC.makers
            return $ show m
    , dir "GetTaskCoonfig" 
      $ withData $ \ ( task :: Task ) ->
        return $ toResponse "example config"
    , dir "VerifyTaskConfig" 
      $ withData $ \ ( task :: Task ) ->
        withData $ \ ( conf :: Config ) ->
        return $ toResponse $ unlines
               [ "verification result"
               , show task
               , show conf
               ]
    , dir "GetTaskInstance" 
      $ withData $ \ ( task :: Task ) ->
        withData $ \ ( conf :: Config ) ->
        return $ toResponse "some instance"
    , dir "GradeTaskSolution" 
      $ withData $ \ ( task :: Task ) ->
        withData $ \ ( inst :: Instance ) ->
        withData $ \ ( sol  :: Solution ) ->
        return $ toResponse "aha"
    ]

