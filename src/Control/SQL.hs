module Control.SQL 

( squery, query, logged, reed
, myconnect, collectRows, disconnect
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
import Data.Typeable
import Text.ParserCombinators.Parsec.Expr

import Mysqlconnect

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
    -- appendFile logfile cs ; appendFile logfile strich 
    return ()

strich = "\n--------------------------------\n"

reed cs = case readsPrec 0 cs of
    [(x, "")] -> x
    ( sonst :: [(a,String)] ) -> 
	error $ unlines [ "kein parse."
			, "für eingabe:", cs 
			, "für typ:" , show (typeOf (undefined::a)) 
			, "readsPrec:" , show sonst
			]

--------------------------------------------------------------------------------

data Id = Id [ String ] deriving Typeable

instance T.ToDoc Id where
    toDoc (Id ids) = 
        T.sepBy (T.char '.') $ map text ids

instance R.Reader Id where
    reader = do
        ids <- R.my_identifier `R.sepBy1` R.my_dot 
        return $ Id ids

--------------------------------------------------------------------------------

data Query = Query Action [ Modifier ]  deriving Typeable

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

--------------------------------------------------------------------------------

-- data Bind = Bind  Expression (Maybe Id)   deriving Typeable
data Bind = Bind Id (Maybe Id)   deriving Typeable

instance T.ToDoc Bind where
    toDoc (Bind e mi) = case mi of
		  Nothing -> T.toDoc e
	          Just i  -> T.hsep [ T.toDoc e, T.text "AS", T.toDoc i ]

instance R.Reader Bind where
    reader = do 
         e <- R.reader
	 mi <- R.option Nothing $ do 
               R.my_reserved "AS" <|> R.my_reserved "as" 
	       fmap Just R.reader
	 return $ Bind e mi 


----------------------------------------------------------------------------

data Action = Select [ Bind ]
	    | Insert Id [(Id, Expression)]
	    | Update Id [(Id, Expression)]
	    | Delete Id
  deriving Typeable

instance T.ToDoc Action where
    toDoc (Select bs) = T.text "SELECT" 
	       <+> T.sepBy T.comma ( map T.toDoc bs )
    toDoc (Insert tab pairs) = T.text "INSERT" <+> T.vcat
        [ T.text "INTO" <+> T.toDoc tab <+> T.dutch_tuple ( map ( T.toDoc . fst ) pairs )
	, T.text "VALUES" <+> T.dutch_tuple ( map ( T.toDoc . snd ) pairs )
	]
    toDoc (Update tab pairs) = T.text "UPDATE" <+> T.toDoc tab 
         <+> T.text "SET" <+> T.sepBy T.comma ( do
         (e, v) <- pairs
	 return $ hsep [ T.toDoc e, T.equals, T.toDoc v ]
       )
    toDoc (Delete tab) = T.text "DELETE" <+> T.text "FROM" <+> T.toDoc tab

instance R.Reader Action where
    reader = do 
       R.my_reserved "SELECT" ; bs <- reader `R.sepBy` R.my_comma ; return $ Select bs 
          -- TODO: complete this

instance Show Action where show      = render . T.toDoc
instance Read Action where readsPrec = R.parsec_readsPrec

----------------------------------------------------------------------------

data Modifier = From [ Id ]
	      | Where Expression
              | Using [ Bind ]
     deriving Typeable

instance T.ToDoc Modifier where
    toDoc (From ids) = T.text "FROM" <+> T.sepBy T.comma ( map T.toDoc ids )
    toDoc (Where e)  = T.text "WHERE" <+> T.toDoc e
    toDoc (Using b)  = T.text "USING" <+> T.sepBy T.comma ( map T.toDoc b )

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
		| EFun Id [ Expression ]
		| EBinop String Expression Expression -- ^ completely parenthesized
        deriving Typeable


quote :: String -> String
quote cs = do
    c <- cs
    if c `elem` [ '\'', '"', '\\', '`' ]
       then [ '\\' , c ]
       else [ c ]

instance T.ToDoc Expression where
    toDoc (EId id) = T.toDoc id
    toDoc (EInteger i) = T.toDoc i

    -- NOTE: this should NEVER cut off a string (max_string_length)
    toDoc (EString s) = T.doubleQuotes $ T.text $ quote s

    -- note: open par must come immediately after function symbol (no <+>)
    toDoc (EFun fun args) = T.toDoc fun <> T.dutch_tuple ( map T.toDoc args )

    toDoc (EBinop "BETWEEN" x (EBinop "AND" y z)) 
	= T.parens $ T.fsep [ T.toDoc x, T.text "BETWEEN"
			    , T.toDoc y, T.text "AND", T.toDoc z 
			    ]
    toDoc (EBinop op x y) = T.parens $ T.fsep [ T.toDoc x, T.text op, T.toDoc y ]

instance R.Reader Expression where
    reader = buildExpressionParser operators atomic

atomic =   R.my_parens reader
     R.<|> id_or_fun
     R.<|> do { i <- R.my_integer ; return $ EInteger i }
     R.<|> do { s <- R.my_stringLiteral ; return $ EString s }

id_or_fun = do
    id <- reader
    args <- option Nothing $ R.my_parens $ fmap Just $ reader `R.sepBy` R.my_comma
    return $ case args of
        Nothing -> EId id 
	Just xs -> EFun id xs

operators = 
    let lop cs = op cs (EBinop cs) AssocLeft
        op name f  =
            Infix ( do { R.my_symbol name; return f } R.<?> "operator" ) 
    in [ map lop [ "*", "/" ]
       , map lop [ "+", "-" ]
       , map lop [ "<", "=", ">" ]
       , map lop [ "BETWEEN" ] -- ?
       , map lop [ "AND", "OR" ]
       ]

-----------------------------------------------------------------------------------

class ToEx a where 
      toEx :: a -> Expression

-----------------------------------------------------------------------------------

equals :: Expression -> Expression -> Expression
equals = EBinop "="

ands :: [ Expression ] -> Expression
ands [] = Control.SQL.equals (EInteger 0) (EInteger 0)
ands xs = foldr1 (EBinop "AND") xs
