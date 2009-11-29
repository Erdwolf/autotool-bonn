{-# language TemplateHaskell, DeriveDataTypeable #-}

module Program.Cexp.Annotated where

import qualified Program.Cexp.Type as T

import Data.Map ( Map )
import qualified Data.Map as M
import Autolib.FiniteMap

import Control.Monad ( forM_ )

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Util.Size
import Data.Typeable
import Data.Tree
import qualified Data.List

data Annotated = 
     Annotated { node :: Node
               , lvalue :: Maybe T.Identifier
               , rvalue :: Maybe Integer
               , actions :: [ (Time, Action) ]
               , children :: [ Annotated ]
               }
    deriving Typeable

instance Size Annotated where
    size = succ . sum . map size . children

blank :: Annotated
blank = Annotated { children = [], lvalue = Nothing, rvalue = Nothing, actions = [] }

newtype Time = Time Int deriving ( Eq, Ord )
data Action = Read T.Identifier | Write T.Identifier Integer deriving ( Eq, Ord )

data Node = Literal Integer | Symbol T.Identifier | Operator T.Oper

$(derives [makeToDoc,makeReader] [''Node, ''Annotated, ''Time, ''Action])

-------------------------------------------------------------------




-------------------------------------------------------------------

-- | produce tree that is partially annotated
start :: T.Exp -> Annotated
start x = case x of
    T.Literal i -> blank { node = Literal i, rvalue = Just i }
    T.Symbol  s -> blank { node = Symbol s, lvalue = Just s }
    T.Apply op xs -> blank { node = Operator $ T.oper op 
                           , children = map start xs
                           }

-------------------------------------------------------------------

all_actions :: Annotated -> [ (Time, Action) ]
all_actions a = actions a ++ ( children a >>= all_actions )

type Store = FiniteMap T.Identifier Integer

-- | produce sequence of stores (changes on each assignment)
execute :: Annotated -> Reporter ( Map Time Store )
execute a = do
    let acts = Data.List.sort $ all_actions a

    let distinct ( x : y : zs ) = 
            if fst x == fst y 
            then reject $ text "gleichzeitige Aktionen" 
                        </> vcat [ toDoc x, toDoc y ]
            else distinct ( y : zs )
        distinct _ = return ()
    distinct acts

    let run st [] = []
        run st ( (time, act) : rest ) = 
            let st' = case act of
                    Read p -> st
                    Write p x -> M.insert p x st
            in  ( time, st' ) : run st' rest
    let stores = M.fromList $ run emptyFM acts

    return stores

-------------------------------------------------------------------

check :: Map Time Store -> Annotated -> Reporter ()
check stores a = case node a of

    Literal i -> do
        case lvalue a of
            Just _ -> reject $ text "Literal ist kein lvalue" </> toDoc a
            Nothing -> return ()
        case rvalue a of
            Just v | i /= v -> reject $ text "rvalue falsch" </> toDoc a
            _ -> return ()
        when ( not $ null $ actions a ) $ reject
            $ text "keine Aktionen in Literalknoten" </> toDoc a

    Symbol s -> do
        case lvalue a of
            Just v | s /= v -> reject ( text "lvalue falsch" </> toDoc a )
            _ -> return ()
        case rvalue a of
            Nothing -> do
                when ( not $ null $ actions a ) $ reject
                     $ text "rvalue nicht benutzt => keine Aktionen in Symbolknoten" </> toDoc a
            Just v -> do
                p <- case lvalue a of
                    Just p -> return p
                    Nothing -> reject $ text "lvalue erforderlich" </> toDoc a
                w <- case actions a of
                    [ ( time, Read q ) ] -> do
                        when ( p /= q ) 
                             $ reject $ text "Read benutzt falschen lvalue" </> toDoc a
                        store <- case M.lookup time stores of
                             Just store -> return store
                             Nothing -> reject $ text "Zeitpunkt unbekannt" </> toDoc a
                        return $ case M.lookup p store of
                             Just val -> val
                             Nothing -> 0 -- FIXME
                    _ -> reject $ text "Bestimmung des rvalues erfordert Read-Aktion"
                when ( v /= w ) $ reject 
                     -- gemein, wenn wir hier die Speicherbelegung nicht anzeigen
                     $ text "rvalue falsch" </> toDoc a

    Operator op -> do
        undefined

-----------------------------------------------------

same_skeleton :: (T.Exp, Annotated) -> Reporter ()
same_skeleton (x, a) = case (x,node a) of
    (T.Symbol xs, Symbol ys) ->
        when ( xs /= ys ) 
             $ mismatch (text "verschiedene Bezeichner") x a
    (T.Literal i, Literal j) ->
        when ( i /= j ) 
             $ mismatch (text "verschiedene Literale") x a
    (T.Apply xop xargs, Operator cs ) -> do
        when ( T.oper xop /= cs) 
             $ mismatch (text "verschiedene Operatoren") x a
        when ( length xargs /= length ( children a )) 
             $ mismatch (text "verschiede Argumentanzahlen") x a
        forM_ ( zip xargs $ children a ) $ same_skeleton

mismatch msg x a = reject $ 
    msg </> vcat [ text "original" </> toDoc a
                 , text "annotiert" </> toDoc x 
                 ]

-------------------------------------------------------------------

