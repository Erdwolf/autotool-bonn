import Paths_autotool_test

import Data.List
import qualified Text.ParserCombinators.Parsec as P
import Data.Monoid
import qualified Data.Map as M

import System.FilePath
import System.Directory
import System.Environment
import Text.Printf

import Text.XHtml hiding ((</>), (<->))
import Control.Monad

type Result = String

data Tree a b = Node b | Nest a (M.Map String (Tree a b)) deriving Show

instance Functor (Tree a) where
   f `fmap` Node b = Node (f b)
   f `fmap` Nest a m = Nest a (fmap f `fmap` m)

fmap2 :: (a -> c) -> (b -> d) -> Tree a b -> Tree c d
fmap2 f g = fmap g . go where
    go (Node a)   = Node a
    go (Nest a m) = Nest (f a) (go `fmap` m)

merge :: (Monoid a, Monoid b) => Tree a b -> Tree a b -> Tree a b
merge (Node a) (Node b) = Node (a `mappend` b)
merge (Nest a as) (Nest b bs) = Nest (a `mappend` b) (M.unionWith merge as bs)

parseLine :: P.Parser (Tree () String)
parseLine = do
    P.string "-> "
    path <- parsePath
    P.string ": "
    result <- P.many P.anyChar
    return $ path (Node result)

parsePath :: P.Parser (Tree () String -> Tree () String)
parsePath = do
    elem <- P.many1 (P.satisfy (\c -> c /= '/' && c /= ':'))
    do P.char '/'
       path <- parsePath
       return (Nest () . M.singleton elem . path)
     P.<|> do
       return (Nest () . M.singleton elem)

parseFile :: String -> Tree () String
parseFile s = foldl merge (Nest mempty M.empty)
    [t | l <- lines s,
         Right t <- [P.runParser parseLine () "" l]]

data TestResult = TestResult {
    rpass :: Int, -- green  : perfect
    rgood :: Int, -- yellow : ok, but with differences
    rfail :: Int, -- red    : fail
    rskip :: Int  -- grey   : skipped
 }

instance Monoid TestResult where
    mempty = TestResult 0 0 0 0
    TestResult p g f s `mappend` TestResult p' g' f' s'
        = TestResult (p + p') (g + g') (f + f') (s + s')

score :: Tree () String -> Tree TestResult (TestResult, String)
score t = collect $ (fmap scoreNode t) where
    result (Node (r, _)) = r
    result (Nest r _)    = r
    collect (Nest _ as) = let as' = fmap collect as
                          in  Nest (mconcat $ map result $ M.elems as') as'
    collect (Node n) = Node n
    scoreNode str
        | "Ok "     `isPrefixOf` str' = (mempty { rpass = 1 }, str)
        | "(Ok) "   `isPrefixOf` str' = (mempty { rgood = 1 }, str)
        | "Skipped "`isPrefixOf` str' = (mempty { rskip = 1 }, str)
        | otherwise                   = (mempty { rfail = 1 }, str)
       where str' = str ++ " "

labelt :: Ord l => l -> Tree a b -> Tree (M.Map l a) (M.Map l b)
labelt l = fmap2 (M.singleton l) (M.singleton l)

main :: IO ()
main = do
    (dir:args) <- getArgs
    files <- forM args $ \fn -> do
        (labelt fn . score . parseFile) `fmap` readFile fn
    createDirectoryIfMissing True dir
    setCurrentDirectory dir
    datadir <- getDataDir
    copyFile (datadir </> "test.css") "test.css"
    let scores = foldr merge (Nest mempty M.empty) files
    dump args "" scores
    return ()

dump :: [String]
     -> FilePath
     -> Tree (M.Map String TestResult) (M.Map String (TestResult, String))
     -> IO (M.Map String (TestResult, String))
dump args dir (Node m) = do
    createDirectoryIfMissing True dir
    writeFile (dir </> "index.html") $
        showHtml (leafAsHtml dir args m)
    return m
dump args dir (Nest result ms) = do
    tests <- forM (M.assocs ms) $ \(name, tree) -> do
        ((,) name) `fmap` dump args (dir </> name) tree
    createDirectoryIfMissing True dir
    let result' = fmap (\r -> (r, showResult r)) result
    writeFile (dir </> "index.html") $
        showHtml (indexAsHtml dir args result' tests)
    return result'

leafAsHtml
    :: FilePath
    -> [String]
    -> M.Map String (TestResult, String)
    -> Html
leafAsHtml dir args result
    = myHeader dir +++
      (body $ table $ toHtml $ aboves $ map besides $ [
          [cell (td (toHtml arg)),
           cell (td (toHtml text)
               ! [thestyle  ("background-color: " ++ colorResult res)])]
      | arg <- args,
        let (res, text) = maybe (mempty, "") id (M.lookup arg result)
      ])

indexAsHtml
    :: FilePath
    -> [String]
    -> M.Map String (TestResult, String)
    -> [(String, M.Map String (TestResult, String))]
    -> Html
indexAsHtml dir args result tests
    = myHeader dir +++
      (body $ (! [theclass "index"]) $ table $ toHtml
            $ headline `above` summary `above` contents)
  where
    headline = besides (cell (th (toHtml "name"))
        : map (cell . th . toHtml) args)
    summary  = besides (cell (td (toHtml "total") ! [theclass "summary"])
        : map (cell . (! [theclass "summary"]) . htmlResult
                    . maybe (mempty, "") id . flip M.lookup result) args)
    contents = aboves $ map besides $ [
            cell (td (anchor (toHtml test) ! [href (test </> "index.html")])) :
            [ cell . htmlResult
                   . maybe (mempty, "skipped") id . flip M.lookup row $ name
            | name <- args]
        | (test, row) <- tests]

myHeader :: FilePath -> Html
myHeader dir = header $
    (thelink noHtml ! [href css, rel "stylesheet"]) +++
    (thetitle (toHtml $ "tests: " ++ dir)) +++
    (meta ! [httpequiv "Content-Type", content "text/html;charset=utf-8"])
  where
    css = (joinPath $ map (const "..") $ splitDirectories dir) </> "test.css"

htmlResult :: (TestResult, String) -> Html
htmlResult (r@(TestResult 0 0 0 s), ann) = td (annotation ann +++ noHtml)
    ! [thestyle "background-color: grey"]
htmlResult (r@(TestResult p g f s), ann) = td (annotation ann +++ toHtml (pcnt (p+g) (p+g+f)))
    ! [thestyle ("text-align: right; background-color: " ++ colorResult r)]

showResult :: TestResult -> String
showResult (TestResult p g f s) = printf
    "%d pass / %d good / %d fail; %d total, %d skipped" p g f (p + g + f) s

colorResult :: TestResult -> String
colorResult (TestResult p g f s) = let
    t = fromIntegral (p + g + f + s)
    yy = fromIntegral g / t
    rr = fromIntegral f / t
    gg = fromIntegral p / t
    gh = fromIntegral s / t
  in let
    r = cc $ yy + rr + 0.5*gh
    g = cc $ yy + gg + 0.5*gh
    b = cc $           0.5*gh
    cc :: Double -> Int
    cc c = round (255 * c)
  in
    printf "#%02x%02x%02x" r g b

pcnt :: Int -> Int -> String
pcnt a b = printf "%3.2f%%" ((100 :: Double) * fromIntegral a / fromIntegral b)

annotation :: String -> Html
annotation = (! [theclass "annn"]) . thediv . (! [theclass "ann"]) . thediv
    . (! [theclass "an"]) . table . tr . td . toHtml
