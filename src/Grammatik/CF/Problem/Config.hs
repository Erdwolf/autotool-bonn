module Grammatik.CF.Problem.Config where

-- $Id$

import Reporter.Type
import Language.Type
import Grammatik.Type

data Config =
     Config { lang :: Language
	    , typ :: Grammatik -> Reporter ()  -- form-test (z. b. greibach)
	    , num_samples :: Int -- anzahl der samples
	    , min_sample_length :: Int -- minimale länge der samples
	    }
