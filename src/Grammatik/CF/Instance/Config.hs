module Grammatik.CF.Instance.Config where

-- $Id$

import Language.Type
import Grammatik.Type
import qualified Reporter.Checker

import Reporter
import ToDoc

data Config =
     Config { lang :: Language
	    , typ :: Reporter.Checker.Type Grammatik
	    , yeah :: [ String ]
	    , noh :: [ String ]
	    }

instance ToDoc Config where
    toDoc = toDoc . lang

instance Show Config where show = render . toDoc