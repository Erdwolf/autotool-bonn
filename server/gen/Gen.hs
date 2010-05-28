module Main (main) where

import Types
import Analyse
import JData
import JParser
import JSerializer

import Language.Haskell.Exts
import System.Environment
import Control.Monad
import System.Directory
import System.FilePath

main :: IO ()
main = do
    createDirectoryIfMissing True $ JData.dir
    createDirectoryIfMissing True $ JParser.dir
    createDirectoryIfMissing True $ JSerializer.dir
    getArgs >>= mapM_ processFile

processFile :: String -> IO ()
processFile file = do
    putStrLn $ "processing '" ++ file ++ "'."
    res <-  parseFile file
    case res of
        ParseOk mod -> processModule mod
        _ -> print res

-- Module SrcLoc ModuleName [OptionPragma] (Maybe WarningText)
--   (Maybe [ExportSpec]) [ImportDecl] [Decl]
processModule :: Module -> IO ()
processModule mod = do
    let decls0 = analyseModule mod
        decls = [d | d@(AData (AType nm _) _) <- decls0,
                     not $ nm `elem` blacklist]
    when (null decls) $ putStrLn "  Nothing to do!"
    mapM_ jData decls
    mapM_ jParser decls
    mapM_ jSerializer decls

-- don't generate java code for these types
blacklist :: [String]
blacklist = ["TT", "Config", "Solution", "Description"]
