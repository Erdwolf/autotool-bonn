module Control.SQL 

( squery, query, logged, collectRows, disconnect
, Statement, getFieldValue, getFieldsTypes 
, Query (..), Action (..), Modifier (..)
, Id (..), Bind (..)
, Expression (..), ToEx (..), Control.SQL.equals, ands
)

where

--  $Id$

import Autolib.Reader as R
import Autolib.ToDoc  as T

import Data.List
import Text.ParserCombinators.Parsec.Expr

import Database.MySQL.HSQL hiding ( query, collectRows )
import qualified Database.MySQL.HSQL

--------------------------------------------------------------------------------

-- | structured query
squery :: Connection -> Query -> IO Statement
squery con scom = query con (show scom)

-- | excessive logging
query :: Connection -> String -> IO Statement
query con com = do
    logged com
    Database.MySQL.HSQL.query con com
            `catchSql` \ e -> do 
                   logged $ "query: " ++ (show e) 
		   error $ "SQLqueries.error in query:" ++ (show e) 

collectRows fun stat = do
    i <- Database.MySQL.HSQL.collectRows fun stat
            `catchSql` \ e -> do 
                   logged $ "collectRows: " ++ (show e) 
		   error $ "SQLqueries.error in collectRows: " ++ (show e) 
    logged ( show i )
    return i

logfile = "/tmp/HSQL.log"
logged cs = do
    appendFile logfile cs
    appendFile logfile strich 

strich = "\n--------------------------------\n"


--------------------------------------------------------------------------------

data Id = Id [ String ]

instance T.ToDoc Id where
    toDoc (Id ids) = 
        T.sepBy (T.char '.') $ map text ids

instance R.Reader Id where
    reader = do
        ids <- R.my_identifier `R.sepBy1` R.my_dot 
        return $ Id ids

instance Show Id where show      = render . T.toDoc
instance Read Id where readsPrec = R.parsec_readsPrec

--------------------------------------------------------------------------------

data Query = Query Action [ Modifier ]

instance T.ToDoc Query where
    toDoc (Query a ms) = T.vcat [ T.toDoc a
		     , ( T.nest 4 $ T.vcat $ map T.toDoc ms ) T.<+> T.char ';'
		     ]
instance R.Reader Query where
    reader = do
        a <- reader
        ms <- R.many reader
	R.my_semi
	return $ Query a ms

instance Show Query where show      = render . T.toDoc
instance Read Query where readsPrec = R.parsec_readsPrec

--------------------------------------------------------------------------------
		     
data Bind = Bind [ ( Expression, Maybe Id) ]

instance T.ToDoc Bind where
    toDoc (Bind eis) = T.fsep $ T.punctuate T.comma $ do
             (e, mi) <- eis
             return $ case mi of
		  Nothing -> T.toDoc e
	          Just i  -> T.hsep [ T.toDoc e, T.text "AS", T.toDoc i ]

instance R.Reader Bind where
    reader = fmap Bind $ do 
	         e <- R.reader
		 mi <- R.option Nothing $ do { R.my_reserved "AS" ; fmap Just R.reader }
		 return (e, mi) 
       `R.sepBy` R.my_comma

instance Show Bind where show      = render . T.toDoc
instance Read Bind where readsPrec = R.parsec_readsPrec

----------------------------------------------------------------------------

data Action = Select Bind
	    | Insert Id [(Id, Expression)]
	    | Update Id [(Id, Expression)]
	    | Delete Id

instance T.ToDoc Action where
    toDoc (Select b) = T.text "SELECT" <+> T.toDoc b
    toDoc (Insert tab pairs) = T.text "INSERT" <+> T.vcat
        [ T.text "INTO" <+> T.toDoc tab <+> T.dutch_tuple ( map ( T.toDoc . fst ) pairs )
	, T.text "VALUES" <+> T.dutch_tuple ( map ( T.toDoc . snd ) pairs )
	]
    toDoc (Update tab pairs) = T.text "UPDATE" <+> T.toDoc tab <+> T.sepBy T.comma ( do
         (e, v) <- pairs
	 return $ hsep [ T.toDoc e, T.equals, T.toDoc v ]
       )
    toDoc (Delete tab) = T.text "DELETE" <+> T.text "FROM" <+> T.toDoc tab

instance R.Reader Action where
    reader = do { R.my_reserved "SELECT" ; b <- reader ; return $ Select b }
          -- TODO: complete this

instance Show Action where show      = render . T.toDoc
instance Read Action where readsPrec = R.parsec_readsPrec

----------------------------------------------------------------------------

data Modifier = From [ Id ]
	      | Where Expression
              | Using Bind

instance T.ToDoc Modifier where
    toDoc (From ids) = T.text "FROM" <+> T.sepBy T.comma ( map T.toDoc ids )
    toDoc (Where e)  = T.text "WHERE" <+> T.toDoc e
    toDoc (Using b)  = T.text "USING" <+> T.toDoc b

instance R.Reader Modifier where
    reader = do { R.my_reserved "FROM" ; ids <- many1 reader ; return $ From ids }
         -- TODO: complete this

instance Show Modifier where show      = render . T.toDoc
instance Read Modifier where readsPrec = R.parsec_readsPrec

-------------------------------------------------------------------------------


data Expression = EId Id
		| EInteger Integer
		-- | ETime ClockTime -- TODO
		| EString String
		| EBinop String Expression Expression -- ^ completely parenthesized

instance T.ToDoc Expression where
    toDoc (EId id) = T.toDoc id
    toDoc (EInteger i) = T.toDoc i
--    toDoc (ETime t) = T.toDoc $ show t
    toDoc (EString s) = T.toDoc s
    toDoc (EBinop op x y) = T.parens $ T.fsep [ T.toDoc x, T.text op, T.toDoc y ]

instance R.Reader Expression where
    reader = buildExpressionParser operators atomic

atomic = do { id <- reader ; return $ EId id }
     R.<|> do { i <- R.my_integer ; return $ EInteger i }
     R.<|> do { i <- R.my_integer ; return $ EInteger i }
     R.<|> do { s <- R.my_stringLiteral ; return $ EString s }

operators = 
    let lop cs = op cs (EBinop cs) AssocLeft
        op name f  =
            Infix ( do { R.my_symbol name; return f } R.<?> "operator" ) 
    in [ map lop [ "*", "/" ]
       , map lop [ "+", "-" ]
       , map lop [ "<", "=", ">" ]
       ]

instance Show Expression where show      = render . T.toDoc
instance Read Expression where readsPrec = R.parsec_readsPrec

-----------------------------------------------------------------------------------

class ToEx a where 
      toEx :: a -> Expression

-----------------------------------------------------------------------------------

equals :: Expression -> Expression -> Expression
equals = EBinop "="

ands :: [ Expression ] -> Expression
ands = foldr1 (EBinop "AND")