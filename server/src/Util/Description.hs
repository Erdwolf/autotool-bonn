module Util.Description (
    fromDoc,
    fromToDoc,
    fromOutput,
    help
) where

import Info
import Types.Description
import Util.Xml.Output

import qualified Autolib.Multilingual as AM
import qualified Autolib.Output as AO
import qualified Autolib.ToDoc as AT
import qualified Gateway.Help as GH

import Text.XHtml
import Data.Typeable

fromToDoc :: AT.ToDoc a => a -> Description
fromToDoc = fromDoc . AT.toDoc

fromDoc :: AT.Doc -> Description
fromDoc = fromOutput . AO.Doc

fromOutput :: AO.Output -> Description
fromOutput = DString . outputToXmlString

help :: (Data.Typeable.Typeable a) => a -> Description
help = fromOutput . GH.help
