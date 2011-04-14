{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

-- | Grammatik als Akzeptor, d. h. aus dem Wort wird das Startsymbol abgeleitet
-- mit umgekehrten Pfeilen.

module Grammatik.Machine where

import Grammatik.Type
import Grammatik.Property
import Grammatik.Ableitung


import Machine.Class
import Machine.Akzeptieren


import Autolib.Util.Splits
import Autolib.Size

import Control.Monad ( guard )


instance Compute Grammatik Ableitung where
    next g a = mkSet $ do
        let w = car a
        ( pre, midpost ) <- splits w
        ( l, r ) <- setToList $ regeln g
        let ( mid, post ) = splitAt ( length r ) midpost
        guard $ mid == r
        return $ cons ( pre ++ l ++ post ) a
    accepting g a = case car a of
        [ s ] -> s == start g
        _ -> False
    depth g a = size a

instance In Grammatik String Ableitung where
    input_reporter g w = do
        return $ cons w nil

instance Out Grammatik String Ableitung where
    output_reporter g a = do
        return $ car a

