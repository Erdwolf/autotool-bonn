module Main where

import Inter.Types
import Inter.Collector ( makers )

import Network.XmlRpc.Server

list_types :: [ Make ] -> IO [ String ]
list_types makers = return $ do
    Make tag _ _ _ <- makers
    return tag

main :: IO ()
main = cgiXmlRpcServer $ serve makers

serve makers =
     [ ( "autotool.list_types", fun $ list_types makers )
     ]

