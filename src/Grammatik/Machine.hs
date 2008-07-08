{-# language TypeSynonymInstances, MultiParamTypeClasses #-}

-- | Grammatik als Akzeptor, d. h. aus dem Wort wird das Startsymbol abgeleitet
-- mit umgekehrten Pfeilen.

module Grammatik.Machine where

import Grammatik.Type
import Grammatik.Property



import Machine.Class
import Machine.Akzeptieren


import Autolib.Util.Splits

import Control.Monad ( guard )


instance Compute Grammatik String where
    next g w = mkSet $ do
        ( pre, midpost ) <- splits w
        ( l, r ) <- setToList $ regeln g
        let ( mid, post ) = splitAt ( length r ) midpost
        guard $ mid == r
        return $ pre ++ l ++ post
    accepting g w = case w of
        [ s ] -> s == start g
        _ -> False
    depth g w = 0 -- what for?

instance In Grammatik String String where
    input_reporter g w = do
        return w

instance Out Grammatik String String where
    output_reporter g w = do
        return w

