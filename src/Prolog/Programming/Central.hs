{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, ScopedTypeVariables, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances #-}

module Prolog.Programming.Central where

import Prolog.Programming.Prolog
import Prolog.Programming.Data

import Debug ( debug )

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc (derives, makeToDoc, text, vcat, toDoc, nest, ToDoc(..))
import Autolib.Reader (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))
import Autolib.Reporter (reject, inform)
import qualified Autolib.Reporter.IO.Type (reject, inform)
import Data.Typeable (Typeable)
import Inter.Types (OrderScore(..), ScoringOrder(..), direct)

import Data.List ((\\))
import Data.Generics (everything, mkQ)
import Text.Parsec
import Control.Applicative ((<$>),(<*>),(<*))

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

    partial p _ _ = do
        inform $ text "Keine Überprüfung."

    total p (Config cfg) (Facts input) = do
        let Right (spec,facts) = parseConfig cfg
        case consultString (facts ++ input) of
          Left err -> reject $ text $ show err
          Right p -> do
            let answerTo = answer p
            let incorrect = [ query | (query,result) <- spec, not (answerTo query =~= result) ]
            if null incorrect
               then inform $ text "Ja."
               else reject $ vcat [ text "Nein."
                                    , text "Die Anworten auf die folgenden Anfragen sind inkorrekt:"
                                    , nest 4 $ vcat $ map (text.show) incorrect
                                    ]

actual =~= expected =
   (actual `isSublistOf` expected &&
    expected `isSublistOf` actual)

isSublistOf xs ys = xs \\ ys == []

answer p q = removeUnresolvedVariables $ map (flip apply q) $ resolve p q

removeUnresolvedVariables = filter $ (not.) $ everything (||) $ mkQ False (\((i,_) :: VariableName) -> i /= 0)


{- Config parser -}

parseConfig = parse configuration "(config)"

configuration =
   (,) <$> specification <*> sourceText

specification = do
   let startMarker = string "/* "
   let separator   = nl >> string " * "
   let endMarker   = nl >> string " */"
   let line = do
         t  <- term
         char ':' >> optional (char ' ')
         ts <- term `sepBy` string ", "
         return (t,ts)
   startMarker
   line `sepBy` (notFollowedBy endMarker >> separator)  <* endMarker

nl = optional (char '\r') >> newline

sourceText = anyChar `manyTill` eof
