{-# language MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
{-# language DeriveDataTypeable #-}

module Peano.Machine where

import qualified Peano.Data as D
import qualified Peano.Type as T
import qualified Peano.Infer as I
import qualified Peano.Eval as E

import Machine.Class
import Machine.History

import Autolib.Set
import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

newtype Memory = Memory [ Integer ]
    deriving ( Eq, Ord, ToDoc, Reader, Typeable )

instance Encode Memory where
    encode xs = Memory xs

instance Decode Memory where
    decode ( Memory [ x ] ) = x

instance History D.Exp where
    history x = [] 

instance In D.Exp Memory D.Exp  where
    input_reporter p ( Memory xs ) = do
        let x = foldl D.App p $ map D.Const xs
        t <- I.typeof I.std x
        return  x

instance Out D.Exp Memory D.Exp where
    output_reporter p x = do
        v <- E.value E.std x
        case v of 
            E.ValNat n -> return $ Memory [ n ]
            _ -> reject $ text "Resultat vom falschen Typ" </> toDoc v

instance Compute D.Exp D.Exp  where
    next p x = mkSet [ ]
    accepting p x = True
    depth p x = 0

