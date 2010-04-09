module Service.Interface (
    get_server_info,
    get_task_types,
    get_task_description,
    verify_task_config,
    get_task_instance,
    grade_task_solution,
    Server
) where

import Types.TT
import Types.Basic
import Types.Signed as S
import Types.Documented as D
import Types.ServerInfo
import Types.TaskTree
import Types.TaskDescription
import Types.Config
import Types.Instance as I
import Types.Solution

import Network.XmlRpc.Client

import Control.Applicative

type Server = String

get_server_info :: Server -> IO ServerInfo
get_server_info srv =
    unTT <$> remote srv "get_server_info"

get_task_types :: Server -> IO [TaskTree]
get_task_types srv =
    unTT <$> remote srv "get_task_types"

get_task_description :: Server -> Task -> IO TaskDescription
get_task_description srv a =
    unTT <$> remote srv "get_task_description" (TT a)

verify_task_config :: Server
    -> Task -> Config
    -> IO (Either Description (Signed (Task, Config)))
verify_task_config srv a b =
    unTT <$> remote srv "verify_task_config" (TT a) (TT b)

get_task_instance :: Server
    -> Signed (Task, Config) -> Seed
    -> IO (Signed (Task, Instance), Description, Documented Solution)
get_task_instance srv a b =
    unTT <$> remote srv "get_task_instance" (TT a) (TT b)

grade_task_solution :: Server
    -> Signed (Task, Instance) -> Solution
    -> IO (Either Description (Documented Double))
grade_task_solution srv a b =
    unTT <$> remote srv "grade_task_solution" (TT a) (TT b)
