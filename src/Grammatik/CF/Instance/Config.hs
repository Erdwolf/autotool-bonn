module Grammatik.CF.Instance.Config where

-- $Id$

import Language.Type
import Grammatik.Type

import Reporter
import ToDoc

data Config =
     Config { lang :: Language
	    , typ :: Grammatik -> Reporter ()  -- form-test (z. b. greibach)
	    , yeah :: [ String ]
	    , noh :: [ String ]
	    }

instance ToDoc Config where
    toDoc = toDoc . lang

instance Show Config where show = render . toDoc