{-# LANGUAGE NoMonomorphismRestriction #-}

import Types.Basic
import Types.TaskTree
import Types.Version
import Types.ServerInfo
import Types.TaskDescription
import qualified Types.Documented as D
import qualified Types.Signed as S
import Types.Solution
import Types.Config
import Util.Xml.Output

import Autolib.CGI
import Happstack.Server
import Control.Monad
import Control.Monad.Trans

import qualified Autolib.Output as O
import qualified Autolib.Output.XHtml as O
import qualified Autolib.Multilingual as M

import Service.Interface
import Data.List
import Data.Word
import qualified Text.XHtml as X

import qualified Data.String.UTF8 as U
import qualified Data.ByteString.Char8 as B

server :: Server
server = "http://autolat.imn.htwk-leipzig.de/cgi-bin/autotool-0.2.0.cgi"

min_version, max_version :: Version
min_version = Version 0 1 0
max_version = Version 0 2 0

main :: IO ()
main = simpleHTTP nullConf { port = 8000 } $ msum [s1, s2]

s2 = dir "style" $ return $ toResponse $ style

s1 = dir "tool" $ fmap toResponse $ fmap complete $ render $ do
    h1 $ "autOlat test frontend."
    hr -----------------------------------------------------------------
    h2 $ "Server information (get_server_info)"
    info <- liftIO $ get_server_info server
    p $ text $ "Server name: " ++ server_name info
    p $ text $ "Server version: " ++ pp_version (server_version info)
    p $ text $ "Protocol version: " ++ pp_version (protocol_version info)
    when (protocol_version info < min_version ||
          protocol_version info >= max_version) $ do
        p $ text $ "Unsupported Server version."
        mzero
    hr -----------------------------------------------------------------
    h2 $ "Select task type (get_task_types)"
    tasks <- liftIO $ get_task_types server
    task <- table $ select_task_types "Category" tasks
    hr -----------------------------------------------------------------
    h2 $ "Configure task type " ++ task ++ " (get_task_description)"
    taskdescr <- liftIO $ get_task_description server task
    p $ text $ "(Scoring order = " ++ show (task_scoring_order taskdescr) ++ ")"
    let dconf = task_sample_config taskdescr
        CString conf = D.contents dconf
        DString desc = D.documentation dconf
    doc dconf
    configuration <- du `fmap` textarea conf
    p $ menu "" [("Submit", ())]
    hr -----------------------------------------------------------------
    h2 $ "Verify task configuration (verify_task_config)"
    vrfy <- liftIO $ verify_task_config server task (CString configuration)
    config <- case vrfy of
        Left d -> do
            p $ text $ "Error validating configuration:"
            descr d
            mzero
        Right config -> return config
    p $ text $ "Ok."
    hr -----------------------------------------------------------------
    h2 $ "Get a task instance (get_task_instance)"
    p $ text $ "Enter seed:"
    seed <- textfield "123"
    p $ submit "Submit"
    (inst, desc, dsample) <- liftIO $ get_task_instance server config seed
    descr desc
    doc dsample
    let SString sample = D.contents dsample
    sol <- du `fmap` textarea sample
    p $ menu "" [("Submit solution", ())]
    hr -----------------------------------------------------------------
    h2 $ "Verify task solution (grade_task_solution)"
    vrfy <- liftIO $ grade_task_solution server inst (SString sol)
    dscore <- case vrfy of
        Left d -> do
            p $ text $ "Incorrect solution:"
            descr d
            mzero
        Right dscore -> return dscore
    doc dscore
    p $ text $ "Size: " ++ show (D.contents dscore)
    hr -----------------------------------------------------------------
    h2 $ "All done"
    xhtml $ X.anchor (X.toHtml "autOlat project") X.!
        [X.href "http://autolat.imn.htwk-leipzig.de/"]
    return ()

-- select_task_type :: Monad m => TaskTree -> Form m Task
select_task_type (Task task) = return task
select_task_type (Category name subs) = select_task_types name subs

-- select_task_types ::  Monad m => String -> [TaskTree] -> Form m Task
select_task_types name tasks = do
    task <- menu name [(taskName t, t) | t <- tasks]
    select_task_type task

taskName (Task task) =  task
taskName (Category name _) = name ++ "..."

pp_version :: Version -> String
pp_version (Version a b c) = intercalate "." [show a, show b, show c]

pre cs = p $ xhtml $ X.pre $ X.toHtml cs

doc = descr . D.documentation

descr desc = do
    let DString d = desc
    p $ xhtml $ M.specialize M.DE $ O.render $ xmlStringToOutput $ du d

hconcat = foldr (X.+++) X.noHtml

complete doc = hconcat [
    X.header $ hconcat [
        X.thetitle $ X.toHtml "autOlat test interface",
        X.thelink X.noHtml X.! [X.rel "stylesheet", X.href "style"]
    ],
    X.body doc
 ]

style = CSSString $ unlines [
    "table { border: 2px solid #2f62c4; border-collapse: collapse; }",
    "td {  border: 2px solid #2f62c4; }",
    "textarea { background-color: #ddeeff; }"
    ]

newtype CSSString = CSSString String

instance ToMessage CSSString where
    toContentType _ = B.pack "text/css"
    toMessage (CSSString s) = toMessage s

du :: String -> String
du = U.toString . U.fromRep . map (fromIntegral :: Int -> Word8) . map fromEnum

eu :: String -> String
eu = map toEnum . map (fromIntegral :: Word8 -> Int) . U.toRep . U.fromString
