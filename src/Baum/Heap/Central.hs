{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Baum.Heap.Central where


import Baum.Heap.Config
import Baum.Heap.Class
import Baum.Heap.Op
import Baum.Heap.Inter
import Baum.Heap.Generate
import qualified Tree

-- import qualified Baum.Binary as B
-- import qualified Baum.ZweiDrei as Z

import Challenger.Partial
import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Dot
import Autolib.Reader

import Inter.Types
import Inter.Quiz
import Data.Typeable

newtype T t = T t deriving (Typeable)

instance Show t => Show (T t) where
    show (T t) = show t

instance Read t => Read (T t) where
    readsPrec d s = [(T t, s') | (t, s') <- readsPrec d s]

instance OrderScore (T t) where
    scoringOrder _ = None

instance ( Tag t baum a ) => 
    Measure (T t) ( Instanz baum a ) [ Op a ] where
    measure t inst ops = fromIntegral $ length ops

instance ( Tag t baum a ) => 
    Partial (T t) ( Instanz baum a ) [ Op a ] where

    report _ ( start, plan, end ) = do
       inform $ text "Auf den Baum:"
       peng start
       inform $ vcat 
	      [ text "sollen diese Operationen angewendet werden"
              , text "(wobei Sie  Any  geeignet ersetzen sollen):"
              , nest 4 $ toDoc plan
              , text "so daß dieser Baum entsteht:"
              ]
       peng end

    initial _ ( start, plan, end ) =
        plan

    total   _ ( start, plan, end ) ops = do
        inform $ text "Beginne mit"
	peng start
        c <- steps start plan ops
	inform $ text "Stimmt überein mit Aufgabenstellung?"
        peng end
	assert ( c `equal` end) $ Autolib.ToDoc.empty


instance Tag t baum a
      => Generator (T t) ( Config a ) ( Instanz baum a ) where
    generator t conf key = Baum.Heap.Generate.generate conf

instance Project (T t) ( Instanz baum a ) ( Instanz baum a ) where
    project t i = i

baum_type :: Tag t baum Int => t -> baum Int
baum_type = undefined

make_fixed :: ( Tag t baum Int ) => t -> Make
make_fixed t = 
  let start = (  flip insert 4 $ flip insert 3 
            $ Baum.Heap.Class.empty )
            `asTypeOf`  baum_type t 
      end = deleteMin $ flip insert 2
          $ start
  in  direct ( T t ) ( start 
                     , [ Any, Any ] :: [ Op Int ] 
                     , end 
                     )

make_quiz :: ( Tag t baum Int ) => t -> Make
make_quiz t = 
  quiz (T t) Baum.Heap.Config.example  




