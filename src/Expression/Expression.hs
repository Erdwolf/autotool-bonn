module Expressive where

import ExpressionType
import Reader

class Expressive a where 
      toExp :: a -> Expression
      fromExp :: Expression -> a




	     














