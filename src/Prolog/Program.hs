module Prolog.Program where

import Prolog.Data

import qualified Autolib.Reader as R
import qualified Autolib.ToDoc as T

import Prelude hiding ( head )
import qualified Prelude

data Clause = 
     Clause { head :: Term 
            , body :: Terms
            }

data Program = 
     Program { clauses :: [ Clause ]
             , query :: Terms
             }

instance T.ToDoc Clause where
    toDoc c = 
        if null $ body c 
        then T.toDoc ( head c ) T.<+> T.text "."
        else ( T.toDoc ( head c ) T.<+> T.text ":-" ) 
             T.</> ( toDoc_body $ body c )

instance R.Reader Clause where
    reader = do
        h <- R.reader
        ts <-    do R.my_symbol ":-" ; reader_body
           R.<|> do R.my_symbol "."  ; return []
        return $ Clause { head = h, body = ts }   

reader_body = do 
    ts <- R.my_commaSep R.reader
    R.my_symbol "." 
    return ts

instance T.ToDoc Program where
    toDoc p = T.vcat ( map T.toDoc $ clauses p ) 
        T.$$ ( T.text "?-" T.<+> toDoc_body ( query p ) )

toDoc_body ts = ( T.fsep $ T.punctuate T.comma $ map T.toDoc ts ) 
           T.<+> T.text "."

instance R.Reader Program where
    reader = do
        cs <- R.many R.reader
        R.my_symbol "?-"
        q <- R.my_commaSep R.reader
        R.my_symbol "."
        return $ Program { clauses = cs, query = q }
