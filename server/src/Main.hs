{-# LANGUAGE TemplateHaskell, FlexibleInstances, UndecidableInstances,
  GeneralizedNewtypeDeriving #-}

module Main where

import Inter.Collector
import Inter.Types
import Gateway.Help

import Data.Autolib.Transport
import Data.Autolib.Transport.HaXR
import qualified Autolib.ToDoc as AT
import qualified Autolib.Output as AO
import qualified Autolib.Multilingual as AM
import Text.XHtml

import Network.XmlRpc.Server
import Network.XmlRpc.Internals
import Control.Monad
import qualified Data.Tree as T

data ServerInfo = ServerInfo {
    protocol_version :: Version,
    server_name      :: String,
    server_version   :: Version
    -- further fields may be added in future protocol versions
}

data Version = Version {
    major :: Int,
    minor :: Int,
    micro :: Int
}

data TaskTree
    = Task     Task
    | Category Name [TaskTree]

type Name = String
type Task = String

data Documented a = Documented a String
data Config       = Config String

$(derives [makeToTransport] [''Documented, ''Config, ''Version, ''ServerInfo, ''TaskTree])

main :: IO ()
main = cgiXmlRpcServer proto

proto :: [(String, XmlRpcMethod)]
proto = [
    ("get_server_info", fun get_server_info),
    ("get_task_types", fun get_task_types),
    ("get_task_description", fun get_task_description)
    ]

get_server_info :: IO (TT ServerInfo)
get_server_info = return $ TT $ ServerInfo {
    protocol_version = Version { major = 0, minor = 0, micro = 0 },
    server_name = "autotool",
    server_version = Version { major = 0, minor = 0, micro = 0 }
 }

get_task_types :: IO (TT [TaskTree])
get_task_types = return $ TT $ map mkTaskTree (T.subForest tmakers) where
    mkTaskTree (T.Node (Left label) sub) = Category label (map mkTaskTree sub)
    mkTaskTree (T.Node (Right task) [])  = Task (show task)

get_task_description :: TT Task -> IO (TT (Documented Config))
get_task_description (TT name) = TT `fmap` do
    let (m:_) = [m | m@(Make _ name' _ _ _) <- makers, name == name']
    case m of
        Make _ _ _ _ conf -> return $ Documented
            (Config (AT.render . AT.toDoc $ conf))
            (renderHtml' . AM.specialize AM.UK . AO.render . help $ conf)

-- verify_task_config :: Task -> Config -> Signed Config
-- verify_task_config = undefined

-- get_task_instance  :: Task -> Signed Config -> Seed -> Pair (Documented (Signed Instance)) (Documented Solution)
-- get_task_instance = undefined

-- grade_task_solution :: Task -> Signed Instance -> Solution -> Documented (Pair Bool Double)
-- grade_task_solution = undefined

renderHtml' :: Html -> String
renderHtml' = renderHtml

newtype TT a = TT { unTT :: a } deriving ToTransport

instance ToTransport a => XmlRpcType (TT a) where
    toValue = encode . toTransport . unTT
    fromValue = errorToErr . fmap TT . (fromTransport <=< decode)
    getType _ = TUnknown

errorToErr :: Monad m => Error a -> Err m a
errorToErr (Error  e) = fail e
errorToErr (Result x) = return x

{-
putStrLn (showJSValue (Data.Autolib.Transport.encode . toTransport $ v) "")
<methodCall>
  <methodName>get_server_info</methodName>
</methodCall>
<methodCall>
  <methodName>get_task_types</methodName>
</methodCall>
-}