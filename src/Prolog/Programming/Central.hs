{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, TupleSections #-}

module Prolog.Programming.Central where

import Prolog.Programming.Prolog (term, apply, resolve, consultString, VariableName(..), Term)
import Prolog.Programming.Data

import Debug ( debug )

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc (derives, makeToDoc, text, vcat, hsep, toDoc, nest, ToDoc(..))
import Autolib.Reader (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))
import Autolib.Reporter (reject, inform)
import qualified Autolib.Reporter.IO.Type (reject, inform)
import Control.Monad (filterM)
import Control.Monad.Trans (liftIO)
import Control.Exception (evaluate)
import System.Timeout (timeout)
import Data.Typeable (Typeable)
import Inter.Types (OrderScore(..), ScoringOrder(..), direct)

import Data.List ((\\), nub)
import Data.Generics (everything, mkQ)
import Text.Parsec
import Control.Applicative ((<$>),(<*>),(<*))

rejectIO = Autolib.Reporter.IO.Type.reject
informIO = Autolib.Reporter.IO.Type.inform

data Prolog_Programming = Prolog_Programming deriving Typeable

$(derives [makeReader, makeToDoc] [''Prolog_Programming])

make_fixed = direct Prolog_Programming $ Config ""


instance OrderScore Prolog_Programming where
    scoringOrder h = Increasing

instance Verify Prolog_Programming Config where
    verify _ (Config _) = do
        -- TODO Do some verification.
        return ()

instance Partial Prolog_Programming Config Facts where
    describe p (Config cfg) = text $
      either (\_->"Fehler in Aufgaben-Konfiguration!")
             snd
             (parseConfig cfg)

    initial p _ = Facts ""

    totalIO p (Config cfg) (Facts input) = do
        let Right (specs,facts) = parseConfig cfg

        case consultString (facts ++ "\n" ++ input) of
          Left err -> rejectIO $ text $ show err
          Right p -> do
            let answerTo = answer p
            let check (QueryWithAnswers query result) = answerTo query =~= result
                check (StatementToCheck query)        = resolve p [query] /= []
                check (Hidden spec)                   = check spec
            incorrect <- liftIO $ concatMap (\(s,mbb) -> maybe [Timeout s] (\b -> if b then [] else [s]) mbb) <$> sequence [ (s,) <$> timeout 1000000 (evaluate (check s)) | s <- specs ]
            let explain (QueryWithAnswers query _) = vcat [ text $ show query, nest 4 $ vcat [ text "Ihre Lösung liefert:", text $ show $ answerTo query ] ]
                explain (StatementToCheck query)   =        text $ show query
                explain (Hidden _)                 =        text "(ein versteckter Test)"
                explain (Timeout x)                = hsep [ explain x, text "*scheint nicht zu terminieren*" ]
            if null incorrect
               then informIO $ text "Ja."
               else rejectIO $ vcat [ text "Nein."
                                  , text "Die Anworten auf die folgenden Anfragen sind inkorrekt:"
                                  , nest 4 $ vcat $ map explain incorrect
                                  ]

actual =~= expected =
   (nub actual `isSublistOf` nub expected &&
    nub expected `isSublistOf` nub actual)

isSublistOf xs ys = xs \\ ys == []

answer p q = removeUnresolvedVariables $ map (flip apply q) $ resolve p [q]

removeUnresolvedVariables = filter $ (not.) $ everything (||) $ mkQ False $ \(VariableName i _) -> i /= 0


{- Config parser -}

parseConfig = parse configuration "(config)"

configuration =
   (,) <$> specification <*> sourceText

data Spec = QueryWithAnswers Term [Term] | StatementToCheck Term | Hidden Spec | Timeout Spec

specification = do
   let startMarker = string "/* "
   let separator   = string "* "
   let endMarker   = string "*/"
   let line = option id (char '!' >> return Hidden) <*> do
         t <- term
         (do char ':' >> optional (char ' ')
             ts <- term `sepBy` string ", "
             return (QueryWithAnswers t ts))
          <|> return (StatementToCheck t)
   startMarker
   line `sepBy` (notFollowedBy endMarker >> separator) <* endMarker

sourceText = anyChar `manyTill` eof
