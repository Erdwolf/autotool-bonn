module Uni.SS04 where

--  $Id$

import Inter.Types

--import qualified Uni.SS04.Serie1
--import qualified Uni.SS04.Serie2
--import qualified Uni.SS04.Serie3
--import qualified Uni.SS04.Serie4
import qualified Uni.SS04.Serie5
import qualified Uni.SS04.Factor


-- hier steht ALLES machbare drin
-- in der datenbank steht dann, zu welcher zeit es erlaubt ist.

boiler :: IO [ Variant ]
boiler = sequence -- der erste ist der default-wert!       
        $  []
--       ++ Uni.SS04.Serie1.generate
--       ++ Uni.SS04.Serie2.generate
--       ++ Uni.SS04.Serie3.generate
--       ++ Uni.SS04.Serie4.generate
         ++ Uni.SS04.Serie5.generate
         ++ [ do i <- Uni.SS04.Factor.generate ; return $ Variant i ]

