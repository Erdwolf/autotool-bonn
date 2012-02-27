{-# LANGUAGE TemplateHaskell, QuasiQuotes, MultiParamTypeClasses, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, TupleSections #-}

module Prolog.Programming.Central where

import Language.Prolog (term, apply, resolve, consultString, VariableName(..), Term(..))
import Prolog.Programming.Data

import Debug ( debug )

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc (derives, makeToDoc, text, vcat, hsep, toDoc, nest, ToDoc(..))
import Autolib.Reader (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))
import Autolib.Reporter (reject, inform)
import qualified Autolib.Reporter.IO.Type (reject, inform)
import Control.Monad (filterM, forM)
import Control.Monad.Trans (liftIO)
import Control.Exception (evaluate)
import System.Timeout (timeout)
import Data.Typeable (Typeable)
import Inter.Types (OrderScore(..), ScoringOrder(..), direct)

import Data.List ((\\), nub, isPrefixOf)
import Data.Generics (everything, mkQ)
import Text.Parsec hiding (Ok)
import Control.Applicative ((<$>),(<*>),(<*))
import Control.Arrow ((>>>),(***),(&&&),first,second,arr)

import Prolog.Programming.GraphViz (resolveWithTree, asInlinePng)


rejectIO = Autolib.Reporter.IO.Type.reject
informIO = Autolib.Reporter.IO.Type.inform

data Prolog_Programming = Prolog_Programming deriving Typeable

$(derives [makeReader, makeToDoc] [''Prolog_Programming])

make_fixed = direct Prolog_Programming $ Config $ unlines
   [ "/* a_predicate(Foo,Bar): a_predicate(expected_foo1,expected_bar1), a_predicate(expected_foo2,expected_bar2)"
   , " * a_statement_that_has_to_be_true"
   , " * !a_predicate_whose_answers_are_hidden(Foo,Bar): a_predicate(expected_foo1,expected_bar1), a_predicate(expected_foo2,expected_bar2)"
   , " * !a_hidden_statement_that_has_to_be_true"
   , " * §a_test_with_resolution_tree(X) % Only shown if test fails."
   , " * #a_negative_test_with_resolution_tree(X) % Only shown if test fails, e.g. the statement is true."
   , " */"
   , "/* Everything from here on (up to an optional hidden section separated by a line of 3 or more dashes)"
   , " * will be part of the visible exercise description (In other words: Only the first comment block is special)."
   , " * "
   , " * You can add as many tests as you like, but keep Autotool's time limit in mind. Additionally, every test has its own time limit,"
   , " * so if one of your tests does not terminate (soon enough) this will be reported as a failure (mentioning the timeout)."
   , " * "
   , " * In this visible part, you can place the explanation of the exercise and all facts & clauses you want to give to the student."
   , " */"
   , "a_fact."
   , "a_clause(Foo) :- a_clause(Foo)."
   , "a_dcg_rule --> a_dcg_rule, [terminal1, terminal2], { prolog_term }."
   , "a_test_with_resolution_tree(left_branch) :- fail. % See test line 5"
   , "a_test_with_resolution_tree(right_branch) :- fail. % See test line 5"
   , "/*"
   , " * The program text will be concatenated with whatever the student submits."
   , " */"
   , "------------------------------"
   , "/*"
   , " * This is also part of the program, but is not presented to the student."
   , " * "
   , " * Be careful to avoid naming clashes to not confuse the student with error messages about code they can't see."
   , " */"
   ]

instance OrderScore Prolog_Programming where
    scoringOrder h = Increasing

instance Verify Prolog_Programming Config where
    verify _ (Config cfg) = do
        either (fail . show) (\_ -> return ()) $ parseConfig cfg

instance Partial Prolog_Programming Config Facts where
    describe p (Config cfg) = text $
      either (\_->"Fehler in Aufgaben-Konfiguration!")
             (\(_,(visible_facts,_)) -> visible_facts)
             (parseConfig cfg)

    initial p _ = Facts ""

    totalIO p (Config cfg) (Facts input) = do
        let Right (specs,(visible_facts,hidden_facts)) = parseConfig cfg
            facts = visible_facts ++ "\n" ++ hidden_facts

        case consultString (facts ++ "\n" ++ input) of
          Left err -> rejectIO $ text $ show err
          Right p -> do
            let check (QueryWithAnswers query expected) =
                           case solutions p query of { Right (actual,_) | actual =~= expected -> Ok; Right (actual,_) -> WrongResult actual; Left err -> ErrorMsg err }
                check (WithTree (QueryWithAnswers query expected)) =
                           case solutions p query of { Right (actual,_) | actual =~= expected -> Ok; Right (actual,tree) -> Tree tree (WrongResult actual); Left err -> ErrorMsg err }
                check (StatementToCheck query) =
                           case solutions p query of { Right ([],_) -> Wrong; Right _ -> Ok; Left err -> ErrorMsg err }
                check (WithTree (StatementToCheck query)) =
                           case solutions p query of { Right ([],tree) -> Tree tree (Wrong); Right _ -> Ok; Left err -> ErrorMsg err }
                check (WithTreeNegative (StatementToCheck query)) =
                           case solutions p query of { Right ([],_) -> Ok; Right (_,tree) -> TreeNegative tree (Wrong); Left err -> ErrorMsg err }
                check (Hidden _ spec)                   = check spec
            incorrect <- liftIO $ concatMap (\(s,mbb) -> maybe [(s,Timeout)] (\r -> case r of { Ok -> []; _ -> [(s,r)] }) mbb) <$> sequence [ (s,) <$> timeout 10000000 (evaluate (check s)) | s <- specs ]
            let explain x              Timeout              = hsep [ describe x, text "*scheint nicht zu terminieren*" ]
                explain x              (ErrorMsg msg)       = vcat [ describe x, indent [ text "Folgender Fehler ist aufgetreten:", text $ msg ] ]
                explain x@(Hidden _ _) _                    = describe x
                explain x              (WrongResult actual) = vcat [ describe x, indent [ text "Ihre Lösung liefert:", indent [ text $ show $ actual ] ] ]
                explain x              Wrong                = describe x
                explain x              (Tree tree r)        = vcat [ explain x r, indent [ text "Ableitungsbaum (Anklicken zum Vergrößern):", indent [ text $ asInlinePng tree ] ] ]
                explain x              (TreeNegative tree r)= vcat [ explain x r, indent [ text "Fälschlicherweise auftretender Ableitungsbaum (Anklicken zum Vergrößern):", indent [ text $ asInlinePng tree ] ] ]
                describe (QueryWithAnswers query _) = text $ show query
                describe (StatementToCheck query)   = text $ show query
                describe (Hidden str _)             = text $ "(ein versteckter Test" ++ str ++")"
                describe (WithTree x)               = describe x
                describe (WithTreeNegative x)       = describe x <+> text " sollte nicht herleitbar sein."
            if null incorrect
               then informIO $ text "Ja."
               else rejectIO $ vcat [ text "Nein."
                                    , text "Die Antworten auf die folgenden Anfragen sind inkorrekt:"
                                    , indent (map (uncurry explain) incorrect)
                                    ]

indent = nest 4 . vcat

data TestResult r e g = Ok
                      | Wrong
                      | WrongResult r
                      | ErrorMsg e
                      | Timeout
                      | Tree g (TestResult r e g)
                      | TreeNegative g (TestResult r e g)

(=~=) :: [Term] -> [Term] -> Bool
actual =~= expected =
   (nub actual `isSublistOf` nub expected &&
    nub expected `isSublistOf` nub actual)
 where
  isSublistOf xs ys = xs \\ ys == []

solutions p q = first (map (`apply` q)) <$> resolveWithTree p [q]


{-
resolveTerm p q = do
   us <- resolve p [q]
   return $ removeUnresolvedVariables $ map (flip apply q) us
 where
  removeUnresolvedVariables = filter $ (not.) $ everything (||) $ mkQ False $ \(VariableName i _) -> i /= 0
-}

{- Config parser -}

parseConfig = parse configuration "(config)"

configuration =
   (,) <$> specification <*> sourceText

data Spec = QueryWithAnswers Term [Term]
          | StatementToCheck Term
          | Hidden String Spec
          | WithTree Spec
          | WithTreeNegative Spec

specification = do
   let specLine = (.) <$> withTreeFlag <*> hiddenFlag <*> do
         t <- term
         (do char ':' >> optional (char ' ')
             ts <- term `sepBy` string ", "
             return (QueryWithAnswers t ts))
          <|> return (StatementToCheck t)
   lines <- commentBlock
   zip [1..] lines `forM` \(i,s) -> do
      case parse specLine ("Specification line " ++ show i) s of
         Right spec -> return spec
         Left err   -> fail (show err)
 where
   hiddenFlag   = option id (char '!' >> Hidden <$> option "" (string "(\"" >> anyChar `manyTill`  string "\")"))
   withTreeFlag = option id (char '§' >> return WithTree)
              <|> option id (char '#' >> return WithTreeNegative)

commentBlock = do
   let startMarker =            string "/* "
   let separator   = newline >> string " * "
   let endMarker   = newline >> string " */"
   let line = many $ do notFollowedBy separator
                        notFollowedBy endMarker
                        anyToken
   between startMarker endMarker $ line `sepBy` try separator

sourceText = do
    ls <- lines <$> anyChar `manyTill` eof
    let (visible, hidden) = breakWhen ("---" `isPrefixOf`) ls
    return (unlines visible, unlines hidden)

breakWhen :: (a -> Bool) -> [a] -> ([a],[a])
breakWhen p = (takeWhile (not . p) &&& dropWhile (not . p)) >>> second (drop 1)
