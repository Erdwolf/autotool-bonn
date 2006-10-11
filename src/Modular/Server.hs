module Main where

import Modular.Documented
import Modular.Signed
import Modular.Config
import Modular.Task
import Modular.Instance
import Modular.Solution

import Inter.Types
import Inter.Collector ( makers )

import Network.XmlRpc.Server

import Autolib.ToDoc
import Autolib.Reporter

import Control.Monad ( guard )

list_types :: [ Make ] -> IO [ Task ]
list_types makers = return $ do
    Make tag _ _ _ <- makers
    return $ Task tag

get_config :: [ Make ] 
	   -> Task 
	   -> IO ( Documented Config )
get_config makers task = do
    let xconfs = do 
            Make this _ _ conf <- makers
	    guard $ Modular.Task.contents task == this
	    return $ Documented 
		   { Modular.Documented.contents = 
		         Config { Modular.Config.contents = show conf }
		   , documentation = "see online doc at URL ..."
		   }
    case xconfs of
        [ xconf ] -> return xconf
	[]     -> error "no task with this name"
	xconfs -> error "more than one task with this name"

verify_config :: [ Make ] 
	      -> Task
	      -> Config 
	      -> IO ( Signed Config ) 
verify_config makers task conf = do
    let actions = do 
            Make this _ verify ( _ :: conf ) <- makers
	    guard $ Modular.Task.contents task == this
	    -- FIXME: check for parse errors
	    return $ verify $ read $ Modular.Config.contents conf
    case actions of
        [ action ] -> do
	    let ( result, doc :: Doc ) = export action
	    case result of
	        Just () -> sign conf
		Nothing -> error $ show doc
	[]      -> error "no task with this name"
	actions -> error "more than one task with this name"

get_instance :: [ Make ]
	     -> Task
	     -> Signed Config
	     -> IO ( Documented ( Signed Instance ) )
get_instance makers task sconf = do
    undefined

get_hint :: [ Make ]
	 -> Task
	 -> Signed Config
	 -> Signed Instance
	 -> IO Solution
get_hint makers task sconf sinst = do
    undefined

grade :: [ Make ]
      -> Task
      -> Signed Config
      -> Signed Instance
      -> Solution
      -> IO ( Documented ( Bool, Int ) )
grade makers task sconf sinst sol = do
    undefined

main :: IO ()
main = cgiXmlRpcServer $ serve makers

serve makers =
     [ ( "autotool.list_types", fun $ list_types makers )
     , ( "autotool.get_config", fun $ get_config makers )
     , ( "autotool.verify_config", fun $ verify_config makers )
     ]
  

