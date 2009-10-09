module Util.Description (
    fromDoc,
    help
) where

import Info
import Types.Description

import qualified Autolib.Multilingual as AM
import qualified Autolib.Output as AO
import qualified Autolib.ToDoc as AT
import qualified Gateway.Help as GH

import Text.XHtml
import Data.Typeable

fromToDoc :: AT.ToDoc a => a -> Description
fromToDoc = fromDoc . AT.toDoc

fromDoc :: AT.Doc -> Description
fromDoc = DString . AT.render

fromHtml :: AM.Type Html -> Description
fromHtml = DString . renderHtml . AM.specialize language

help :: (Data.Typeable.Typeable a) => a -> Description
help = fromHtml . AO.render . GH.help
