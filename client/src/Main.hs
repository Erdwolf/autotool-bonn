import Service.Interface

import Types.Config
import Types.Solution
import Types.TaskDescription
import Util.Xml.Output

import System.Environment
import System.Exit

main :: IO ()
main = do
    args <- getArgs
    server <- case args of
        [server] -> return server
        _ -> do
            putStrLn "usage: autolat-client <URL>"
            exitWith (ExitFailure 1)

    putStrLn "\n=== Find server info ===\n"
    print =<< get_server_info server

    putStrLn "\n=== Get task types ===\n"
    print =<< get_task_types server

    putStrLn "\n=== Find task description ===\n"
    let task = "Convert_To_Exp-Quiz"
    print =<< get_task_description server task

    putStrLn "\n=== Configure task ===\n"
    let config = "Quiz { generate = [ Alphabet (mkSet \"ab\"), Max_Size 5 ]\n\
                 \     , solve    = [ Alphabet (mkSet \"ab\"), Simple ] }"
    res <- verify_task_config server task (CString config)
    print res

    case res of
        Right signedTaskConfig -> do
            putStrLn "\n=== Get a task instance ===\n"
            let seed = "test"
            res2@(signedTaskInstance, DString desc, _) <-
                get_task_instance server signedTaskConfig seed
            print res2

            print (xmlStringToOutput desc)

            do  putStrLn "\n=== Send an invalid solution ===\n"
                let solution = ""
                print =<< grade_task_solution server signedTaskInstance
                                              (SString solution)

            do  putStrLn "\n=== Send another solution - at time of testing, it was valid. ===\n"
                let solution = "((a+bb)(a+b))^*b(ab+b)^*"
                print =<< grade_task_solution server signedTaskInstance
                                              (SString solution)

        _ -> return ()
