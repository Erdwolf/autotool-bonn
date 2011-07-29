{-# LANGUAGE DeriveDataTypeable, ViewPatterns #-}
module Prolog.Programming.Prolog where
import Control.Monad.Identity
import Control.Monad
import Control.Arrow (second)
import Data.Generics (Data(..), Typeable(..), everywhere, mkT, everything, mkQ)
import Data.List (intercalate)
--
import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Control.Applicative ((<$>),(<*>),(<$),(<*))
--
import GHC.Exts (IsString(..))

data Term = Struct Atom [Term]
          | Var VariableName
          | Wildcard
          | Cut
      deriving (Eq, Data, Typeable, Ord)
var = Var . VariableName 0

data Clause = Clause { lhs :: Term, rhs_ :: [Goal] }
            | ClauseFn { lhs :: Term, fn :: [Term] -> [Goal] }
      deriving (Data, Typeable)
rhs (Clause   _ rhs) = const rhs
rhs (ClauseFn _ fn ) = fn

data VariableName = VariableName Int String
      deriving (Eq, Data, Typeable, Ord)

type Atom         = String
type Unifier      = [Substitution]
type Substitution = (VariableName, Term)
type Program      = [Clause]
type Goal         = Term

instance Show Term where
   show t@(Struct "." [_,_]) =
      let (ts,rest) = g [] t in
         --if all isChar ts
         --   then "\"" ++ concat (map show ts) ++ (if isNil rest then "" else "|" ++ show rest) ++  "\""
         "[" ++ intercalate "," (map show ts) ++ (if isNil rest then "" else "|" ++ show rest) ++  "]"
    where g ts (Struct "." [h,t]) = g (h:ts) t
          g ts t = (reverse ts, t)
          isNil (Struct "[]" []) = True
          isNil _                = False
   show (Struct a []) = a
   show (Struct a ts) = a ++ "(" ++ intercalate ", " (map show ts) ++ ")"
   show (Var v)       = show v
   show Wildcard      = "_"
   show Cut           = "!"

instance Show VariableName where
   show (VariableName 0 v) = v
   show (VariableName i v) = show i ++ "#" ++ v

instance Show Clause where
   show (Clause   lhs [] ) = show $ show lhs
   show (Clause   lhs rhs) = show $ show lhs ++ " :- " ++ intercalate ", " (map show rhs)
   show (ClauseFn lhs _  ) = show $ show lhs ++ " :- " ++ "<Haskell function>"

unify, unify_with_occurs_check :: MonadPlus m => Term -> Term -> m Unifier

unify = fix unify'

unify_with_occurs_check =
   fix $ \self t1 t2 -> if (t1 `occursIn` t2 || t2 `occursIn` t1)
                           then fail "occurs check"
                           else unify' self t1 t2
 where
   occursIn t = everything (||) (mkQ False (==t))


unify' _ Wildcard _ = return []
unify' _ _ Wildcard = return []
unify' _ (Var v) t  = return [(v,t)]
unify' _ t (Var v)  = return [(v,t)]
unify' self (Struct a1 ts1) (Struct a2 ts2) | a1 == a2 && same length ts1 ts2 =
    unifyList self (zip ts1 ts2)
unify' _ _ _ = mzero

same :: Eq b => (a -> b) -> a -> a -> Bool
same f x y = f x == f y

unifyList _ [] = return []
unifyList unify ((x,y):xys) = do
   u  <- unify x y
   u' <- unifyList unify (map (both (apply u)) xys)
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
   , ClauseFn (Struct "<" [var "N", var "M"]) (binaryIntegerPredicate (<))
   , ClauseFn (Struct ">" [var "N", var "M"]) (binaryIntegerPredicate (>))
   , ClauseFn (Struct "=<" [var "N", var "M"]) (binaryIntegerPredicate (<=))
   , ClauseFn (Struct ">=" [var "N", var "M"]) (binaryIntegerPredicate (>=))
   , ClauseFn (Struct "=:=" [var "N", var "M"]) (binaryIntegerPredicate (==))
   , ClauseFn (Struct "is" [var "L", var "R"]) is
   , Clause (Struct "member" [var "X", Struct "." [var "X", var "Rest"]]) []
   , Clause (Struct "member" [var "X", Struct "." [Wildcard, var "Rest"]])
                [Struct "member" [var "X", var "Rest"]]
   , ClauseFn (Struct "=.." [var "Term", var "List"]) univ
   , ClauseFn (Struct "atom" [var "T"]) atom
   , ClauseFn (Struct "char_code" [var "Atom", var "Code"]) char_code
   , Clause (Struct "phrase" [var "RuleName", var "InputList"])
               [Struct "phrase" [var "RuleName", var "InputList", Struct "[]" []]]
   , Clause (Struct "phrase" [var "RuleName", var "InputList", var "Rest"])
               [ Struct "=.." [var "Goal", foldr cons nil (var "RuleName" : arguments [{- TODO -}] (var "InputList") (var "Rest"))]
               , var "Goal"
               ]
   , Clause (Struct "append" [Struct "[]" [], var "YS", var "YS"]) []
   , Clause (Struct "append" [Struct "." [var "X", var "XS"], var "YS", Struct "." [var "X", var "XSYS"]]) [Struct "append" [var "XS", var "YS", var "XSYS"]]
   ]
 where
   binaryIntegerPredicate :: (Integer -> Integer -> Bool) -> ([Term] -> [Goal])
   binaryIntegerPredicate p [Struct (reads->[(n,"")]) [], Struct (reads->[(m,"")]) []] | (n :: Integer) `p` (m :: Integer) = []
   binaryIntegerPredicate p _ = [Struct "false" []]

   is [t, eval->Just n] = [Struct "=" [t, Struct (show n) []]]
   is _                 = [Struct "false" []]

   eval (Struct (reads->[(n,"")]) []) = return n :: Maybe Integer
   eval (Struct "+" [t1, t2])   = (+) <$> eval t1 <*> eval t2
   eval (Struct "-" [t1, t2])   = (-) <$> eval t1 <*> eval t2
   eval (Struct "mod" [t1, t2]) = mod <$> eval t1 <*> eval t2
   eval (Struct "-" [t])        = negate <$> eval t
   eval _                       = mzero

   univ [Struct a ts, list]                        = [Struct "=" [Struct "." [Struct a [], foldr cons nil ts], list]]
   univ [term,        Struct "." [Struct a [], t]] = [Struct "=" [term, Struct a (foldr_pl (:) [] t)]]
   univ _                                          = [Struct "false" []]

   atom [Struct _ []] = []
   atom _             = [Struct "false" []]

   char_code [Struct [c] [], t]               = [Struct "=" [Struct (show (fromEnum c)) [], t]]
   char_code [t, Struct (reads->[(n,"")]) []] = [Struct "=" [t, Struct [toEnum n] []]]
   char_code _                                = [Struct "false" []]


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
            return (unifier, rhs clause (map snd unifier))

         renameVars = everywhere (mkT (\(VariableName _ v) -> VariableName depth v)) :: Clause -> Clause

      cut ((_,_,[]):stack) = cut stack
      cut (_:stack) = stack

      choose depth _ _  []              stack = backtrack depth stack
      choose depth u gs ((u',gs'):alts) stack =
         let u'' = u +++ u' in
         resolve' (succ depth) u'' (map (apply u'') $ gs' ++ gs) ((u,gs,alts) : stack)

      backtrack _     [] =
         fail "Goal cannot be resolved!"
      backtrack depth ((u,gs,alts):stack) =
         choose (pred depth) u gs alts stack

u1 +++ u2 = simplify $ u1 ++ u2

simplify :: Unifier -> Unifier
simplify u = map (second (apply u)) u


{- Parser -}
consult = fmap consultString . readFile

consultString :: String -> Either ParseError Program
consultString = parse (whitespace >> program <* eof) "(input)"

program = many (clause <* char '.' <* whitespace)

whitespace = skipMany (comment <|> skip space)
comment = skip $ choice
   [ string "/*" >> (manyTill anyChar $ try $ string "*/")
   , char '%' >> (manyTill anyChar $ try $ skip newline <|> eof)
   ]

skip = (>> return ())

clause = do t <- struct <* whitespace
            dcg t <|> normal t
   where
      normal t = do
            ts <- option [] $ do string ":-" <* whitespace
                                 terms
            return (Clause t ts)

      dcg t = do
            string "-->" <* whitespace
            ts <- terms
            return (translate (t,ts))

      translate ((Struct a ts), rhs) =
         let lhs' = Struct a (arguments ts (head vars) (last vars))
             vars = map (var.("d_"++).(a++).show) [0..length rhs] -- We explicitly choose otherwise invalid variable names
             rhs' = zipWith3 translate' rhs vars (tail vars)
         in Clause lhs' rhs'

      translate' t s s0 | isList t   = Struct "=" [ s, foldr_pl cons s0 t ] -- Terminal
      translate' (Struct a ts)  s s0 = Struct a (arguments ts s s0)         -- Non-Terminal

arguments ts xs ds = ts ++ [ xs, ds ]
-- arguments ts xs ds = [ xs \\ ds ] ++ ts

--infix 6 \\
--x \\ y = Struct "\\" [x,y]

isList (Struct "." [_,_]) = True
isList (Struct "[]" [])   = True
isList _                  = False

foldr_pl f k (Struct "." [h,t]) = f h (foldr_pl f k t)
foldr_pl _ k (Struct "[]" [])   = k


terms = sepBy1 term (char ',' <* whitespace)

term = buildExpressionParser (reverse hierarchy) (bottom <* whitespace)
 where
   hierarchy =
      [ [ binary ";" ]
      , [ prefix "\\+" ]
      , map binary ["<", "=..", "=:=", "=<", "=", ">=", ">", "\\=", "is"]
      , map binary ["+", "-", "\\"]
      , [ binary "*" ]
      , [ binary "mod" ]
      , [ prefix "-" ]
      ]
   bottom = variable
        <|> struct
        <|> list
        <|> stringLiteral
        <|> Cut <$ char '!'
        <|> Struct "{}" . (:[]) <$> between (char '{') (char '}') term
        <|> between (char '(') (char ')') term

   prefix name = Prefix (do{ reservedOp name; return (\t -> Struct name [t]) })
   binary name = Infix (do{ reservedOp name; return (\t1 t2 -> Struct name [t1, t2]) }) AssocRight
   reservedOp = P.reservedOp $ P.makeTokenParser $ emptyDef
      { P.opStart = oneOf ";,<=>\\i*+m"
      , P.opLetter = oneOf "=.:<sod"
      , P.reservedOpNames = [ ";", ",", "<", "=..", "=:=", "=<", "=", ">=", ">", "\\=", "is", "*", "+", "-", "\\", "mod" ]
      , P.caseSensitive = True
      }

variable = (Wildcard <$ char '_' <* notFollowedBy alphaNum)
       <|> Var <$> vname

vname = VariableName 0 <$> ((:) <$> upper    <*> many  alphaNum <|>
                            (:) <$> char '_' <*> many1 alphaNum)

atom = (:) <$> lower <*> many (alphaNum <|> char '_')
   <|> many1 digit
   <|> between (char '\'') (char '\'') (many (noneOf "'"))

struct = do a <- atom
            ts <- option [] $ between (char '(') (char ')') $ sepBy1 term (char ',' <* whitespace)
            return (Struct a ts)

list = between (char '[') (char ']') $
         flip (foldr cons) <$> sepBy term (char ',' <* whitespace)
                           <*> option nil (char '|' >> term)

cons t1 t2 = Struct "."  [t1,t2]
nil        = Struct "[]" []

stringLiteral = foldr cons nil . map (\chr -> Struct (representChar chr) []) <$> between (char '"') (char '"') (try (many (noneOf "\"")))

representChar = show . fromEnum -- This is the classical Prolog representation of chars as code points.
--representChar c = [c] -- This is the more natural representation as one-character atoms.

{- Allow specification through string literals by using OverloadedStrings -}
instance IsString Clause where
   fromString s =
      case parse (clause <* eof) "(Clause literal)" s of
         Left  e -> error (show e)
         Right c -> c
instance IsString Term where
   fromString s =
      case parse (term <* eof) "(Term literal)" s of
         Left  e -> error (show e)
         Right c -> c
