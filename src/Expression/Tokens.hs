--   $Id$

module Expression.Tokens where

import Parsec
import ParsecLanguage
import qualified ParsecToken as P

me            = P.makeTokenParser meDef

lexeme          = P.lexeme me
parens          = P.parens me    
braces          = P.braces me    
brackets          = P.brackets me    
squares          = P.squares me    
semiSep         = P.semiSep me    
semiSep1        = P.semiSep1 me    
commaSep        = P.commaSep me    
commaSep1       = P.commaSep1 me    
whiteSpace      = P.whiteSpace me    
symbol          = P.symbol me    
identifier      = P.identifier me    
reserved        = P.reserved me    
reservedOp      = P.reservedOp me    
natural         = P.natural me    
integer         = P.integer me    
stringLiteral         = P.stringLiteral me    

meDef
    = haskellStyle
        { opStart           = opLetter meDef
        , opLetter          = oneOf ","
	-- muﬂ auch feldangaben wie "@3" verstehen
	, identStart   = letter <|> oneOf "_'@"
	, identLetter    = alphaNum <|> oneOf "_'@"
        }

