module Grammatik.CF.Instance.Config where

-- $Id$

import Language.Type
import Grammatik.Type
import qualified Grammatik.Checker

import Reporter
import ToDoc

data Config =
     Config { lang :: Language
	    , typ :: Grammatik.Checker.Type
	    , yeah :: [ String ]
	    , noh :: [ String ]
	    }

instance ToDoc Config where
    toDoc = toDoc . lang

instance Show Config where show = render . toDoc