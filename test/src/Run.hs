-- | run extracted tests

-- Tests are organized in subdirectories as follows:
--
-- <type>/<task>/raw            -- raw task configuration
-- <type>/<task>/<stud>/instant -- problem instance for student
-- <type>/<task>/<stud>/input   -- student's input
-- <type>/<task>/<stud>/result  -- expected result
--
-- type is the task type, for example CFG-Quiz
-- task is a task number, for example 305
-- stud is a student number, for example 602

import qualified Control.Aufgabe as A
import qualified Control.Types as T

import Inter.Collector
import Inter.Types
import Challenger.Partial
import qualified Autolib.Reporter as R

import qualified Autolib.Reader as P
import qualified Text.ParserCombinators.Parsec as P

import Control.Exception as E
import System.Directory
import System.Environment
import System.FilePath
import System.IO
import Control.Monad
import Data.List
import Data.Function
import Data.Char

import Control.Concurrent

timeout :: Int
timeout = 15000000 -- 15 seconds

main :: IO ()
main = do
    args <- getArgs
    let prefix | null args = ""
               | otherwise = head args
    hSetBuffering stdout LineBuffering
    let m = map head . groupBy ((==) `on` show) . sortBy (compare `on` show) $ makers
    forM_ m $ forMaker prefix

forMaker :: FilePath -> Make -> IO ()
forMaker prefix mk = do
    let dir = show mk
    e <- doesDirectoryExist dir
    when (e && and (zipWith (==) prefix dir)) $ do
        aufs <- getDirectoryContents dir
        forM_ aufs $ forAufgabe prefix mk

forAufgabe :: FilePath -> Make -> String -> IO ()
forAufgabe _ _ auf | auf == "." || auf == ".." = return ()
forAufgabe prefix mk@(Make p tg fun vrfy _) auf = do
    let dir = show mk </> auf
    e <- doesDirectoryExist dir
    when (e && dir /= "." && dir /= ".." && and (zipWith (==) prefix dir)) $ do
        raw <- readFile $ dir </> "raw"
        let aufg = read raw
        when (T.toString (A.typ aufg) /= show mk) $ do
            error $ show mk ++ " /= " ++ T.toString (A.typ aufg) ++ " in " ++ dir
        case readM (T.toString (A.config aufg)) of
            Left err -> putStrLn $
                "skipping " ++ dir ++ " (error parsing config: " ++ err ++ ")"
            Right conf' -> do
                studs <- getDirectoryContents dir
                forM_ studs $ forStudent prefix (Make p tg fun vrfy conf') auf
                -- note that the parsed config was stored in the maker
  `E.catch` \e ->
    putStrLn $
      "skipping " ++ (show mk </> auf) ++ " (caught " ++ show e ++ ")"

forStudent :: FilePath -> Make -> String -> String -> IO ()
forStudent _ _ _ stud | stud == "." || stud == ".." = return ()
forStudent prefix mk auf stud = do
    let dir = show mk </> auf </> stud
    e <- doesDirectoryExist $ dir
    when (e && stud /= "." && stud /= ".." && and (zipWith (==) prefix dir)) $ do
        e' <- doesFileExist $ dir </> "error"
        if e' then putStrLn $ "skipping " ++ dir
             else limited dir $ forTest prefix mk auf stud
  `E.catch` \e ->
    putStrLn $
      "skipping " ++ (show mk </> auf </> stud) ++ " (caught " ++ show e ++ ")"

forTest :: FilePath -> Make -> String -> String -> IO String
forTest _prefix mk@(Make _ _ fun _ conf') auf stud = do
    let dir = show mk </> auf </> stud
    instant <- readFile $ dir </> "instant"
    input <- readFile $ dir </> "input"
    result <- readFile $ dir </> "result"

    let var = fun conf'
        un :: IO (R.Reporter i) -> i
        un = undefined
        i' = readM instant `asTypeOf` Right (un (generate var 42))
        b' = readM input
        w' = readM result
    case (i', b', w') of
        (Right i, Right b, Right w) -> do
             let r = total_neu (problem var) i b
                 msg = compareMsg w (R.result r)
             E.evaluate msg
             return msg
        (Left err, _, _) -> error $ "error parsing instance: " ++ err
        (_, Left err, Right w) -> return $ compareMsg w (Just No) ++ " " ++ err
        (_, Left err, _) -> error $ "error parsing input: " ++ err
        (_, _, Left err) -> error $ "error parsing result: " ++ err

compareMsg :: Maybe Wert -> Maybe Wert -> String
compareMsg a b = case (a, b) of
    (Nothing, Nothing) -> ok
    (Just No, Nothing) -> ok
    (Just No, Just No) -> ok
    (Just Pending, _) -> "Skipped (was pending)"
    (Just (Ok i), Just (Okay j _)) | i == j -> ok
    (Just (Okay i si), Just (Okay j sj)) | i == j ->
        if si == sj then ok else "(Ok) Size mismatch: Got " ++ showX sj ++
            ", wanted " ++ showX si
    _ -> no
  where
    no = "No. Got " ++ showX b ++ ", wanted " ++ showX a
    ok = "Ok"

------------------------------------------------------------------------------

limited :: String -> IO String-> IO ()
limited dir act = do
    let act' = act `E.catch` (\e -> return ("Exception: " ++ showX e))
    res <- newEmptyMVar
    t1 <- forkIO $ act' >>= putMVar res
    t2 <- forkIO $ threadDelay timeout >> putMVar res "Timeout"
    r <- takeMVar res
    killThread t1
    killThread t2
    putStrLn $ "-> " ++ dir ++ ": " ++ r

readM :: P.Reader a => String -> Either String a
readM x = either (Left . showX) Right $ P.runParser (P.parse_complete P.reader) () "" x

showX :: Show a => a -> String
showX = unwords . words . show
