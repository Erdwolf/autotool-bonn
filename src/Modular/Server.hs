module Main where

import Modular.Documented
import Modular.Signed
import Modular.Config
import Modular.Task
import Modular.Instance
import Modular.Seed
import Modular.Solution
import Modular.Pair

import Debug

import Inter.Types
import Control.Types ( size, is_okay )
import Inter.Evaluate
import Inter.Collector ( makers )
import Challenger.Partial

import Network.XmlRpc.Server

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter

import Gateway.Errmsg

import Control.Monad ( guard )
-- import qualified Text.XHtml as H

import qualified Autolib.Multilingual as M
import qualified Autolib.Multilingual.Html as H

import Data.Typeable


find_and_apply tag makers task action = do
    let ms = do 
            m @ ( Make p this _ _ _ ) <- makers
	    guard $ Modular.Task.contents task == this
	    return m
    debug $ "RPC call " ++ tag ++ " starts"
    result <- case ms of
        [ m ] -> action m 
	[]     -> error "no task with this name"
	ms -> error "more than one task with this name"
    debug $ ".. RPC call " ++ tag ++ " ends with result:"
    debug $ show result
    return result

list_types :: [ Make ] -> IO [ Task ]
list_types makers = return $ do
    Make p tag _ _ _ <- makers
    return $ Task tag

get_config :: [ Make ] 
	   -> Task 
	   -> IO ( Documented Config )
get_config makers task = find_and_apply "get_config" makers task 
     $ \ ( Make p _ _ _ conf ) -> do
             return $ Documented 
		   { Modular.Documented.contents = 
		         Config { Modular.Config.contents = show conf }
		   , documentation = "see online doc at URL ..."
		   }

verify_config :: [ Make ] 
	      -> Task
	      -> Config 
	      -> IO ( Signed Config ) 
verify_config makers task conf = find_and_apply "verify_config" makers task
    $ \ ( Make p _ _ verify _ ) -> do
            let input = Modular.Config.contents conf 
            iconf <- lies input
	    let ( result, doc :: Doc ) = export $ verify iconf
	    case result of
	        Just () -> sign conf
		Nothing -> error $ show doc

get_instance :: [ Make ]
	     -> Task
	     -> Signed Config
             -> Seed
	     -> IO ( Pair ( Documented ( Signed Instance ) )
                          ( Documented Solution )
                   )
get_instance makers task sconf seed = find_and_apply "get_instance" makers task 
    $ \ ( Make p _ make _ _ ) -> do
        this <- Modular.Signed.unsign sconf
                 -- FIXME: parse errors
        let conf = read  $ Modular.Config.contents this 
        case make conf of 
            var -> do
                let p = problem var
                g <- generate var ( Modular.Seed.contents seed )
                let ( Just i  , _ :: H.Html ) = export g
                ( _, icom :: H.Html) <- run $ report p i
                si <- sign $ Instance
                           { Modular.Instance.contents = show i
                           , Modular.Instance.tag = show p
                           }
                let start = Challenger.Partial.initial p i 
                return $ Pair
                       { first = Documented
                               { documentation = emit icom
                               , Modular.Documented.contents = si
                               }
                       , second = Documented
                                { documentation = show $ typeOf start 
                                , Modular.Documented.contents = Solution 
                                           { Modular.Solution.contents 
                                                 = show start
                                           }
                                }
                       }

grade :: [ Make ]
      -> Task
      -> Signed Instance
      -> Solution
      -> IO ( Documented ( Pair Bool Double ) )
grade makers task sinst sol = find_and_apply "grade" makers task 
    $ \ ( Make p _ ( _ :: c -> Var p i b ) _ _  ) -> do
       inst <- unsign sinst
       let p :: p = read $ Modular.Instance.tag inst 
       i <- ( lies $ Modular.Instance.contents inst ) :: IO i
       let action = Inter.Evaluate.evaluate p i
                   ( Modular.Solution.contents sol )
       ( res, com :: H.Html) <- run action
       return $ Documented
              { documentation = emit com
              , Modular.Documented.contents = 
                  Pair { first = case res of
                               Nothing -> False
                               Just x -> is_okay x
                       , second = case res of
                               Nothing -> 0
                               Just x -> fromIntegral $ size x
                       }
              }

lies :: Reader a 
      => String -> IO a
lies input = case parse ( parse_complete reader ) "rpc input" input of
	 Left e -> do
              let msg = render $ errmsg 80 e input
	      debug $ "parser input:\n" ++ input
	      debug $ "parser error:\n" ++ msg
	      error $ msg
	 Right x -> return x 



emit h = show $ M.specialize M.UK h


main :: IO ()
main = cgiXmlRpcServer $ serve makers

serve makers =
     [ publish "list_types" $ list_types makers 
     , publish "get_config" $ get_config makers 
     , publish "verify_config" $ verify_config makers 
     , publish "get_instance" $ get_instance makers 
     , publish "grade"  $ grade makers 
     ]
  
publish name action = 
    ( "autotool." ++ name
    , fun action
    )

