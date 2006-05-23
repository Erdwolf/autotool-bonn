module Partition.Beispiel where

import Partition.Param ( Conf ( Conf ) )
import Autolib.Set ( mkSet )

-------------------------------------------------------------------------------

mm :: Conf
mm =  Conf $ mkSet [ 4,5,7,8,10 ]

