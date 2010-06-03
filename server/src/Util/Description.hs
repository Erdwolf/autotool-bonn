-- utility function related to dealing with descriptions

module Util.Description (
    fromDoc,
    fromToDoc,
    fromOutput,
    fromReport,
    help
) where

import Types.Description
import Util.Xml.Output

import qualified Autolib.Output as AO
import qualified Autolib.ToDoc as AT
import qualified Gateway.Help as GH

import Autolib.Reporter

import Data.Typeable

fromToDoc :: AT.ToDoc a => a -> IO Description
fromToDoc = fromDoc . AT.toDoc

fromDoc :: AT.Doc -> IO Description
fromDoc = fromOutput . AO.Doc

fromOutput :: AO.Output -> IO Description
fromOutput = fmap DString . outputToXmlString

fromReport :: Reporter a -> IO Description
fromReport rep = fromOutput (kommentar rep)

help :: (Data.Typeable.Typeable a) => a -> IO Description
help = fromOutput . GH.help
