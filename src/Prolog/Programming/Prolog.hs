{-# LANGUAGE DeriveDataTypeable #-}
module Prolog.Programming.Prolog where
import Control.Monad.Identity
import Control.Monad
import Control.Arrow (second)
import Data.Generics
import Data.List (intercalate)
--
import Text.Parsec
import Control.Applicative ((<$>),(<*>),(<$),(<*))
--
import GHC.Exts (IsString(..))

data Term = Struct Atom [Term]
          | Var VariableName
          | Wildcard
          | Cut
      deriving (Eq, Data, Typeable)
var = Var . VariableName 0

data Clause = Clause { lhs :: Term, rhs :: [Term] }
      deriving (Show, Eq, Data, Typeable)

data VariableName = VariableName Int String
      deriving (Eq, Data, Typeable)

type Atom    = String
type Unifier = [(VariableName, Term)]
type Program = [Clause]
type Goal    = Term

instance Show Term where
   show (Struct a []) = a
   show (Struct a ts) = a ++ "(" ++ intercalate "," (map show ts) ++ ")"
   show (Var v)       = show v
   show Wildcard      = "_"
   show Cut           = "!"

instance Show VariableName where
   show (VariableName 0 v) = v
   show (VariableName i v) = show i ++ "#" ++ v


unify :: MonadPlus m => Term -> Term -> m Unifier
unify Wildcard _ = return []
unify _ Wildcard = return []
unify (Var v) t  = return [(v,t)]
unify t (Var v)  = return [(v,t)]
unify (Struct a1 ts1) (Struct a2 ts2) | a1 == a2 && same length ts1 ts2 =
    unifyList (zip ts1 ts2)
unify _ _ = mzero

same :: Eq b => (a -> b) -> a -> a -> Bool
same f x y = f x == f y


unifyList :: MonadPlus m => [(Term,Term)] -> m Unifier
unifyList [] = return []
unifyList ((x,y):xys) = do
   u  <- unify x y
   u' <- unifyList (map (both (apply u)) xys)
   return (u++u')

both f (x,y) = (f x, f y)


apply :: Unifier -> Term -> Term
apply = flip $ foldl $ flip substitute
  where
    substitute (v,t) (Var v') | v == v' = t
    substitute s     (Struct a ts)      = Struct a (map (substitute s) ts)
    substitute _     t                  = t


builtins :: [Clause]
builtins =
   [ Clause (Struct "="   [var "X", var "X"]) []
   , Clause (Struct "\\=" [var "X", var "X"]) [Cut, Struct "false" []]
   , Clause (Struct "\\=" [var "X", var "Y"]) []
   , Clause (Struct "not" [var "A"]) [var "A", Cut, Struct "false" []]
   , Clause (Struct "not" [var "A"]) []
   , Clause (Struct "\\+" [var "A"]) [var "A", Cut, Struct "false" []]
   , Clause (Struct "\\+" [var "A"]) []
   , Clause (Struct "true" []) []
   , Clause (Struct "," [var "A", var "B"]) [var "A", var "B"]
   , Clause (Struct ";" [var "A", Wildcard]) [var "A"]
   , Clause (Struct ";" [Wildcard, var "B"]) [var "B"]
   ]


resolve :: Program -> [Goal] -> [Unifier]
-- Yield all unifiers that resolve <goal> using the clauses from <program>.
resolve program goals = map cleanup $ resolve' 1 [] goals []
  where
      cleanup = filter ((\(VariableName i _) -> i == 0) . fst)

      resolve' depth usf [] stack =
         (cleanup usf:) $ backtrack depth stack
      resolve' depth usf (Cut:gs) stack =
         resolve' depth usf gs (cut stack)
      resolve' depth usf (nextGoal:gs) stack =
         choose depth usf gs branches stack
       where
         branches = do
            clause <- map renameVars (builtins ++ program) -- NOTE Is it a good idea to "hardcode" the builtins like this?
            unifier <- unify (apply usf nextGoal) (lhs clause)
            return (unifier, rhs clause)

         renameVars = everywhere (mkT (\(VariableName _ v) -> VariableName depth v)) :: Clause -> Clause

      cut ((_,_,[]):stack) = cut stack
      cut (_:stack) = stack

      choose depth _ _  []              stack = backtrack depth stack
      choose depth u gs ((u',gs'):alts) stack =
         resolve' (succ depth) (simplify $ u ++ u') (gs' ++ gs) ((u,gs,alts) : stack)

      backtrack _     [] =
         fail "Goal cannot be resolved!"
      backtrack depth ((u,gs,alts):stack) =
         choose (pred depth) u gs alts stack

simplify :: Unifier -> Unifier
simplify u = map (second (apply u)) u


{- Parser -}

consultString :: String -> Either ParseError Program
consultString = parse (whitespace >> program <* eof) "(input)"

program = many (clause <* char '.' <* whitespace)

whitespace = skipMany (comment <|> skip space)
comment = skip $ choice
   [ string "/*" >> (manyTill anyChar $ try $ string "*/")
   , char '%' >> (manyTill anyChar $ try $ skip newline <|> eof)
   ]

skip = (>> return ())

clause = do t <- struct
            ts <- option [] $ do whitespace >> string ":-"
                                 sepBy1 term (char ',')
            return (Clause t ts)

term = try (do t1 <- term2
               op <- whitespace >> string ";"
               t2 <- term
               return (Struct op [t1,t2]))
   <|> term2

term2 = try (do t1 <- term3
                op <- whitespace >> (string "=" <|> string "\\=")
                t2 <- term2
                return (Struct op [t1,t2]))
    <|> term3

term3 = try (do op <- whitespace >> string "\\+"
                t <- term3
                return (Struct op [t]))
    <|> term4

term4 = (whitespace>>) $
        variable
    <|> struct
    <|> list
    <|> Cut <$ char '!'
    <|> between (char '(') (char ')') term

variable = var <$> ((:) <$> upper <*> many alphaNum)
       <|> Wildcard <$ char '_'

atom = (:) <$> lower <*> many alphaNum
   <|> many1 digit
   <|> between (char '\'') (char '\'') (many (noneOf "'"))

struct = do a <- atom
            ts <- option [] $ between (char '(') (char ')') $ sepBy1 term (char ',')
            return (Struct a ts)



list = between (char '[') (char ']') $
         flip (foldr cons) <$> sepBy term (char ',')
                           <*> option nil (char '|' >> term)
  where
   cons t1 t2 = Struct "."  [t1,t2]
   nil        = Struct "[]" []
