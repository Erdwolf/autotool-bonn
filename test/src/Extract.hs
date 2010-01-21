{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
-- | extract submissions from cache and database.
-- loosely based on Recompute.hs

-- each submission has a "path", of the following path elements:
-- 1) task type,   e.g. Acceptor-NPDA-Direct
-- 2) task number, e.g. 397
-- 3) student id,  e.g. 930
-- usage:
-- extract {path} -> list possible followup path elements
--     "extract Acceptor-NPDA-Direct" will show all task numbers of that type
-- extract {path} '*' -> extract all submissions with the given initial path
--     in particular, "extract '*'"  will extract everything.

import qualified Control.Stud_Aufg as SA
import qualified Control.Aufgabe as A
import qualified Control.Student as S
import Inter.Collector
import Control.Types

import Autolib.ToDoc (toDoc)
import Inter.Types
import Inter.Common

import qualified Autolib.Reader as P
import qualified Text.ParserCombinators.Parsec as P

import Control.Exception
import Data.List
import Data.Function
import System.IO
import System.Directory
import System.Environment
import System.FilePath

import Control.Monad.Reader
import Control.Monad.Trans

data Ctx = Ctx { write :: Bool, directory :: FilePath,
                 path :: [String], verbose :: Bool }

newtype MIO a = MIO { runMIO :: ReaderT Ctx IO a }
    deriving (Monad, MonadIO, MonadReader Ctx)

main :: IO ()
main = do
    args <- getArgs
    let wr = not (null args || last args /= "*")
    mrun forRoot (Ctx { write = wr, directory = ".",
                        path = args, verbose = True })

-- for all makers (task types) ...
forRoot :: MIO ()
forRoot = do
    let m = map head . groupBy ((==) `on` show) . sortBy (compare `on` show) $ makers
    forM_ m $ \mk -> do
        nested (show mk) (forMaker mk)

-- ... and all tasks of that type ...
forMaker :: Make -> MIO ()
forMaker mk = do
    aufs <- io $ A.get_typed (fromCGI (show mk))
    forM_ aufs $ \auf -> do
        nested (show (getANr auf)) $ forAufgabe mk auf

-- ... save the task type to "raw". then, for all instances ...
forAufgabe :: Make -> A.Aufgabe -> MIO ()
forAufgabe mk auf = do
    mWriteFile "raw" $ show $ toDoc auf
    einss <- io $ SA.get_anr (A.anr auf)
    forM_ einss $ \eins -> do
--        hPutStr stderr (show (getSNr eins) ++ "        \r") >> hFlush stderr
        forEinsendung mk auf eins

-- ... save raw database row. then ...
forEinsendung :: Make -> A.Aufgabe -> SA.Stud_Aufg -> MIO ()
forEinsendung mk auf eins@SA.Stud_Aufg { SA.input = Just _ } = do
    studs <- io $ S.get_snr (SA.snr eins)
    -- note: at most one result - SNr is the primary key
    forM_ studs $ \stud -> do
        nested (show (getSNr eins)) $ do
           forEinsendung1 mk auf eins stud
               `mcatch` \exc -> do
                    mWriteFile "error" $ show exc
forEinsendung _ _ _ = return () -- no input, no testcase

-- ... save the corresponding raw database row, instance, input, and result.
forEinsendung1 :: Make -> A.Aufgabe -> SA.Stud_Aufg -> S.Student -> MIO ()
forEinsendung1 (Make _p _tag fun _verify _conf) auf eins stud = do
    mWriteFile "raw" $ show $ toDoc eins
    -- find instance
    (_, instant, _) <- io $
        make_instant_common (A.vnr auf) (Just $ A.anr auf) stud
                            (fun . readM . toString $ A.config auf)
    input <- io $ read_from_file (SA.input eins)
    mWriteFile "instant" (show $ toDoc instant)
    mWriteFile "input" input
    mWriteFile "result" (show $ toDoc $ SA.result eins)

------------------------------------------------------------------------------

-- argument handling and creation of subdirectories
nested :: FilePath -> MIO () -> MIO ()
nested dir action = do
    r <- ask
    case r of
        Ctx { path = [], write = False } ->
            io $ putStrLn dir
        Ctx { path = ps }
            | null ps || head ps == dir || head ps == "*" -> do
                 let r' = r { path = drop 1 ps,
                              directory = directory r </> dir }
                     action' | write r   = subdir dir $ action
                             | otherwise = action
                 local (const r') action'
        Ctx { } ->
            return ()

read_from_file :: Maybe File -> IO String
read_from_file ( Just fname ) = do
    this <- readFile $ toString fname
    return this

getANr :: A.Aufgabe -> Int
getANr auf = let ANr a = A.anr auf in a

getSNr :: SA.Stud_Aufg -> Int
getSNr sauf = let SNr s = SA.snr sauf in s

io :: MonadIO m => IO a -> m a
io = liftIO

mrun :: MIO a -> Ctx -> IO a
mrun a r = runReaderT (runMIO a) r

mcatch :: MIO a -> (SomeException -> MIO a) -> MIO a
mcatch a h = do
    r <- ask
    io $ Control.Exception.catch (mrun a r) (\e -> mrun (h e) r)

subdir :: String -> MIO a -> MIO a
subdir dir a = do
    Ctx { verbose = v } <- ask
    when v $ io $ hPutStr stderr (dir ++ "/") >> hFlush stderr
    r <- a
    let clear = replicate l '\b' ++ replicate l ' ' ++ replicate l '\b'
        l = length dir + 1
    when v $ io $ hPutStr stderr clear >> hFlush stderr
    return r

mWriteFile :: String -> String -> MIO ()
mWriteFile file cts = do
    Ctx { write = w, directory = d } <- ask
    when w $ io $ do
        createDirectoryIfMissing True d
        writeFile (d </> file) cts

readM :: P.Reader a => String -> a
readM x = either (error . show) id $ P.runParser (P.parse_complete P.reader) () "" x
