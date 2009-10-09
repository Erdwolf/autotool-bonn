{-# LANGUAGE TemplateHaskell, FlexibleInstances, UndecidableInstances #-}

module Main where

import Network.XmlRpc.Server

import Service.GetServerInfo
import Service.GetTaskTypes
import Service.GetTaskDescription
-- import Service.VerifyTaskConfig
-- import Service.GetTaskDescription
-- import Service.GradeTaskSolution

main :: IO ()
main = cgiXmlRpcServer proto

proto :: [(String, XmlRpcMethod)]
proto = [
    ("get_server_info", fun get_server_info),
    ("get_task_types", fun get_task_types),
    ("get_task_description", fun get_task_description),
--    ("verify_task_config", fun verify_task_config),
--    ("get_task_instance", fun get_task_instance),
--    ("grade_task_solution", fun grade_task_solution),
    ("ping", fun ping)
    ]

ping :: IO ()
ping = return ()
