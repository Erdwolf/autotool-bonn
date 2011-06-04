{-# LANGUAGE ViewPatterns, TupleSections, DeriveDataTypeable #-}
module Prolog.Programming.Prolog where
import Control.Monad.Identity
import Control.Monad
import Data.Generics
--
import Text.Parsec
import Control.Applicative ((<$>),(<*>),(<$),(<*))
import Data.Char
import Data.List

data Term = Struct Atom [Term]
          | Var VariableName
          | Wildcard
          | Cut
      deriving (Eq, Data, Typeable)

instance Show Term where
   show (Struct a []) = a
   show (Struct a ts) = a ++ "(" ++ intercalate "," (map show ts) ++ ")"
   show (Var (_,v))   = v
   show Wildcard      = "_"
   show Cut           = "!"

data Clause = Clause { lhs :: Term, rhs :: [Term] }
      deriving (Show, Eq, Data, Typeable)

type VariableName = (Int, String)
type Atom         = String

type Unifier = [(VariableName, Term)]
type Program = [Clause]
type Goal    = Term


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
   [ Clause (Struct "="   [Var (0,"X"), Var (0,"X")]) []
   , Clause (Struct "\\=" [Var (0,"X"), Var (0,"X")]) [Cut, Struct "false" []]
   , Clause (Struct "\\=" [Var (0,"X"), Var (0,"Y")]) []
   , Clause (Struct "not" [Var (0,"A")]) [Var (0,"A"), Cut, Struct "false" []]
   , Clause (Struct "not" [Var (0,"A")]) []
   , Clause (Struct "true" []) []
   , Clause (Struct "," [Var (0,"A"), Var (0,"B")]) [Var (0,"A"), Var (0,"B")]
   , Clause (Struct ";" [Var (0,"A"), Wildcard]) [Var (0,"A")]
   , Clause (Struct ";" [Wildcard, Var (0,"B")]) [Var (0,"B")]
   ]


resolve :: Program -> Goal -> [Unifier]
-- Yield all unifiers that resolve <goal> using the clauses from <program>.
resolve program goal = runIdentity $ resolve' 1 [goal]
  where
   resolve' _ [] = return [[]]
   resolve' depth goals = resolve'' (branches goals)
     where
      branches (nextGoal:goals) = do
         clause <- map renameVars (builtins ++ program) -- NOTE Is it a good idea to "hardcode" the builtins like this?
         unifier <- unify nextGoal (lhs clause)
         return (unifier, map (apply unifier) (rhs clause ++ goals))

      renameVars = everywhere (mkT rename) :: Clause -> Clause
         where rename (Var (_,v))   = Var (depth,v)
               rename other         = other

      resolve'' ((unifier, Cut:goals):_) = resolve'' ((unifier, goals):[])
      resolve'' ((unifier, goals):alternatives) =
         (++) <$> (map (unifier ++) <$> resolve' (succ depth) goals) <*> resolve'' alternatives
      resolve'' [] = return []


{- Parser -}

consultString :: String -> Either ParseError Program
consultString = parse (program <* eof) "(input)" . killUnparseableStuff

killUnparseableStuff = filter (not . isSpace)
                     . unlines
                     . filter (not . isPrefixOf "/*")
                     . filter (not . isPrefixOf " *")
                     . filter (not . isPrefixOf "*/")
                     . lines

program = clause `endBy` char '.'

clause = do t <- struct
            ts <- option [] $ do string ":-"
                                 sepBy1 term (char ',')
            return (Clause t ts)

term = try (do t1 <- term2
               op <- operator
               t2 <- term2
               return (Struct op [t1,t2]))
      <|> term2

term2 = var
    <|> struct
    <|> list
    <|> Cut <$ char '!'
    <|> between (char '(') (char ')') term

var = Var.(0,) <$> ((:) <$> upper <*> many alphaNum)
  <|> Wildcard <$ char '_'

atom = (:) <$> lower <*> many alphaNum
   <|> many1 digit

struct = do a <- atom
            ts <- option [] $ between (char '(') (char ')') $ sepBy1 term2 (char ',')
            return (Struct a ts)

operator = foldr1 (<|>) (map string ["=","\\="])

list = between (char '[') (char ']') $
         flip (foldr cons) <$> sepBy term (char ',')
                           <*> option nil (char '|' >> term)
  where
   cons t1 t2 = Struct "|"  [t1,t2]
   nil        = Struct "[]" []
