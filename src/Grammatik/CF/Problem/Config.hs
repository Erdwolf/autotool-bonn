module Grammatik.CF.Problem.Config where

-- $Id$

import Reporter.Type
import Language.Type
import Grammatik.Type
import qualified Grammatik.Checker

data Config =
     Config { lang :: Language
	    , typ :: Grammatik.Checker.Type
	    , num_samples :: Int -- anzahl der samples
	    , min_sample_length :: Int -- minimale länge der samples
	    }
