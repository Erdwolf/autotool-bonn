{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Baum.Such.Central where


import Baum.Such.Config
import Baum.Such.Class
import Baum.Such.Op
import Baum.Such.Generate
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
    Measure (T t) ( Instanz baum a ) (OpList a) where
    measure t inst (OpList ops) = fromIntegral $ length ops

instance ( Tag t baum a ) => 
    Partial (T t) ( Instanz baum a ) (OpList a) where

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
        OpList plan

    total   _ ( start, plan, end ) (OpList ops) = do
        --inform $ text "Beginne mit"
        --peng start
        c <- steps start plan ops
        peng c
        --inform $ text "Stimmt überein mit Aufgabenstellung?"
        peng end
        assert (c `equal` end) $ Autolib.ToDoc.empty

      where

        step b op = do
            --inform $ text "Operation:" <+> toDoc op
            c <- case op of
        	 Insert a -> return $ insert b a
        	 Delete a -> return $ delete b a
        	 _        -> reject $ text "Operation ist unbekannt"
            --inform $ text "Resultat:"
            return c

        steps b [] [] = return b
        steps b [] send = do peng b
                             peng start
                             reject $ vcat
                               [ text "Sie wollen noch diese Operationen ausführen:"
        	                   , nest 4 $ toDoc send
        	                   , text "es sind aber keine mehr zugelassen."
        	                   ]
        steps b plan [] = do peng b
                             peng start
                             reject $ vcat
                               [ text "Es müssen noch diese Operationen ausgeführt werden:"
        	                   , nest 4 $ toDoc plan
        	                   ]
        steps b (p : plan) (s : send) = do
            conforms p s
            c <- step b s
            steps c plan send
          where
            conforms _ Any = do
                peng b
                peng start
                reject $ text "Sie sollen Any durch eine Operation ersetzen."
            conforms Any _ = return ()
            conforms x y | x == y = return ()
            conforms x y | x /= y = do
                peng b
                peng start
                reject $ text "Die Operation" <+> toDoc x <+> text "soll nicht geändert werden." 


instance Tag t baum a
      => Generator (T t) ( Config a ) ( Instanz baum a ) where
    generator t conf key = Baum.Such.Generate.generate conf

instance Project (T t) ( Instanz baum a ) ( Instanz baum a ) where
    project t i = i

make_quiz :: ( Tag t baum Int ) => t -> Make
make_quiz t = quiz (T t) Baum.Such.Config.example  


