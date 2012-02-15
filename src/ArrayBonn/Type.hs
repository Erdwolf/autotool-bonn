module ArrayBonn.Type where

import Autolib.TES.Identifier

import Autolib.Reader
import Autolib.ToDoc

data Statement = Assign Access Exp

data Exp = Reference Access
	| Literal Integer
	| Binary Op Exp Exp

data Op = Add | Subtract | Multiply | Divide

-- | access to array element
data Access = Access Identifier [ Exp ]


