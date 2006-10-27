{-# OPTIONS -fglasgow-exts #-}

module Main where

import Modular.Documented
import Modular.Signed
import Modular.Config
import Modular.Task
import Modular.Instance
import Modular.Seed
import Modular.Solution
import Modular.Pair

import Inter.Types
import Control.Types ( size, is_okay )
import Inter.Evaluate
import Inter.Collector ( makers )
import Challenger.Partial

import Network.XmlRpc.Server

import Autolib.ToDoc
import Autolib.Reporter

import Control.Monad ( guard )
import qualified Text.XHtml as H
import Data.Typeable

find_and_apply makers task action = do
    let ms = do 
            m @ ( Make this _ _ _ ) <- makers
	    guard $ Modular.Task.contents task == this
	    return m
    case ms of
        [ m ] -> action m 
	[]     -> error "no task with this name"
	ms -> error "more than one task with this name"

list_types :: [ Make ] -> IO [ Task ]
list_types makers = return $ do
    Make tag _ _ _ <- makers
    return $ Task tag

get_config :: [ Make ] 
	   -> Task 
	   -> IO ( Documented Config )
get_config makers task = find_and_apply makers task 
     $ \ ( Make _ _ _ conf ) -> do
             return $ Documented 
		   { Modular.Documented.contents = 
		         Config { Modular.Config.contents = show conf }
		   , documentation = "see online doc at URL ..."
		   }

verify_config :: [ Make ] 
	      -> Task
	      -> Config 
	      -> IO ( Signed Config ) 
verify_config makers task conf = find_and_apply makers task
    $ \ ( Make _ _ verify _ ) -> do
            let iconf = read $ Modular.Config.contents conf 
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
get_instance makers task sconf seed = find_and_apply makers task 
    $ \ ( Make _ make _ _ ) -> do
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
                               { documentation = show icom
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
grade makers task sinst sol = find_and_apply makers task 
    $ \ ( Make _ ( _ :: c -> Var p i b ) _ _  ) -> do
       inst <- unsign sinst
       let action = Inter.Evaluate.evaluate 
                   ( read ( Modular.Instance.tag inst ) :: p )
                   ( read ( Modular.Instance.contents inst ) :: i )
                   ( Modular.Solution.contents sol )
       ( res, com :: H.Html) <- run action
       return $ Documented
              { documentation = show com
              , Modular.Documented.contents = 
                  Pair { first = case res of
                               Nothing -> False
                               Just x -> is_okay x
                       , second = case res of
                               Nothing -> 0
                               Just x -> fromIntegral $ size x
                       }
              }


main :: IO ()
main = cgiXmlRpcServer $ serve makers

serve makers =
     [ ( "autotool.list_types", fun $ list_types makers )
     , ( "autotool.get_config", fun $ get_config makers )
     , ( "autotool.verify_config", fun $ verify_config makers )
     , ( "autotool.get_instance", fun $ get_instance makers )
     , ( "autotool.grade", fun $ grade makers )
     ]
  

