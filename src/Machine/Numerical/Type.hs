-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, UndecidableInstances, MultiParamTypeClasses, FlexibleInstances #-} 

module Machine.Numerical.Type where

import qualified Machine.Numerical.Config as Con
import qualified Arithmetic.Op as A

import Autolib.Informed
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter

import Data.Typeable

data Computer = Computer
     deriving Typeable

$(derives [makeReader, makeToDoc] [''Computer])

class    (ToDoc c, Reader c, Con.Check c m) => TypeC c m
instance (ToDoc c, Reader c, Con.Check c m) => TypeC c m

data Type c m = 
     Make { op :: A.Exp Integer
          , key :: Integer
	  , fun_info :: Doc	      -- funktions-name/-erklärung
	  , extra_info :: Doc -- extra erklärung/bedingung
	  , args :: [[ Integer ]]	      -- argumente zum testen
	  , cut :: Int -- höchstens soviele schritte
          , checks :: [c]
	  , start :: m -- damit soll der student anfangen
	  }
     deriving Typeable

-- obsolete fields:

-- zu berechnende funktion
fun :: Type c m -> [ Integer ] -> Reporter Integer
fun mk = \ xs -> A.eval ( mkargs xs ( key mk ) ) ( op mk )

-- sonstige Bedingungen an Maschine
check :: Con.Check c m => Type c m -> m -> Reporter ()
check mk = check_all (checks mk)

check_all :: Con.Check c m
	  => [c] -> m -> Reporter ()
check_all cs = \ m -> sequence_ $ do c <- cs ; return $ Con.check c m

mkargs xs key = A.bind $ ( "mat", key ) : do
         (i, x) <- zip [ 1 :: Int .. ]  xs
         return ( "x" ++ show i , x )

instance Informed ( Type c m ) where
     info m = fun_info m
     informed info m = m { fun_info = info }

$(derives [makeReader, makeToDoc] [''Type])
