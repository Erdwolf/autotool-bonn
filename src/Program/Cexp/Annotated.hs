{-# language TemplateHaskell, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Program.Cexp.Annotated where

import Program.Cexp.Type hiding ( Symbol, Literal )
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
import Data.Maybe ( isJust )
import qualified Data.List

data Annotated = 
     Annotated { node :: Node
               , lvalue :: Maybe Identifier
               , rvalue :: Maybe Integer
               , actions :: [ (Time, Action) ]
               , children :: [ Annotated ]
               }
    deriving Typeable

instance Size Annotated where
    size = succ . sum . map size . children

blank :: Annotated
blank = Annotated { children = [], lvalue = Nothing, rvalue = Nothing, actions = [] }

newtype Time = Time Int deriving ( Eq, Ord, Enum )


data Action = Read  Identifier Integer -- ^  this includes the expected result of the reading
            | Write Identifier Integer 
     deriving ( Eq, Ord )

data Node = Literal Integer | Symbol Identifier | Operator Oper

$(derives [makeToDoc,makeReader] [''Node, ''Annotated, ''Time, ''Action])


-------------------------------------------------------------------

-- | produce tree that is partially annotated
start :: Exp -> Annotated
start x = case x of
    T.Literal i -> blank { node = Literal i, rvalue = Just i }
    T.Symbol  s -> blank { node = Symbol s, lvalue = Just s }
    Apply op xs -> blank { node = Operator $ oper op 
                           , children = map start xs
                           }

-------------------------------------------------------------------

all_actions :: Annotated -> [ (Time, Action) ]
all_actions a = actions a ++ ( children a >>= all_actions )

type Store = FiniteMap Identifier Integer

-- | execute actions in order given by timestamps.
-- checks that Read actions get the values they expect.
execute :: Annotated -> Reporter ()
execute a = do
    let acts = Data.List.sort $ all_actions a

    let distinct ( x : y : zs ) = 
            if fst x == fst y 
            then reject $ text "gleichzeitige Aktionen" 
                        </> vcat [ toDoc x, toDoc y ]
            else distinct ( y : zs )
        distinct _ = return ()
    distinct acts

    let step st ( ta @ (time, act) ) = do
            inform $ toDoc ta
            case act of
                    Read p x -> case M.lookup p st of
                        Nothing -> reject $ text "Wert nicht gebunden"
                        Just y -> 
                            if x == y then return st
                            else reject $ text "Falscher Wert gebunden" 
                    Write p x -> return $ M.insert p x st
    foldM step emptyFM acts
    return ()

-------------------------------------------------------------------

check :: Annotated -> Reporter ()
check a = case node a of

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
                    [ ( time, Read q w) ] -> do
                        when ( p /= q ) 
                             $ reject $ text "Read benutzt falschen lvalue" </> toDoc a
                        return w
                    _ -> reject $ text "Bestimmung des rvalues erfordert Read-Aktion"
                when ( v /= w ) $ reject 
                     $ text "rvalue falsch" </> toDoc a

    Operator op -> do

        forM_ ( children a ) check

        case ( op, children a ) of

            ( Assign, [ l,r ] ) -> do
                p <- case lvalue l of
                    Nothing -> reject $ text "Zuweisungsziel muß lvalue haben" </> toDoc a
                    Just p -> return p
                v <- case rvalue r of
                    Nothing -> reject $ text "Zuweisungsquelle muß rvalue haben" </> toDoc a
                    Just v -> return v
                case actions a of
                    [ (time, Write q w ) ] -> do
                        when ( q /= p ) $ reject $ text "falsches Ziel für Write" </> toDoc a
                        when ( w /= v ) $ reject $ text "falscher Wert für Write" </> toDoc a
                    _ -> reject $ text "Zuweisung erfordert genau eine Write-Aktion" </> toDoc a
        
            ( op , [ l ] ) | elem op [ Prefix  Increment, Prefix  Decrement
                                     , Postfix Increment, Postfix Decrement ] -> do
                when ( isJust $ lvalue a ) $ reject $ text "ist kein lvalue" </> toDoc a
                p <- case lvalue l of
                    Nothing -> reject $ text "Argument muß lvalue haben" </> toDoc a
                    Just p -> return p
                case Data.List.sort $ actions a of
                    [ (rtime, Read q w), (wtime, Write q' w') ] -> do
                        when ( p /= q || p /= q' ) 
                             $ reject $ text "Lese/Schreibziel muß mit lvalue übereinstimmen" </> toDoc a
                        when ( succ rtime /= wtime ) 
                             $ reject $ text "Schreiben muß direkt auf Lesen folgen" </> toDoc a
                        let ( result, store )  = case op of 
                                 Prefix Increment -> ( succ w, succ w )
                                 Postfix Increment ->  ( w, succ w )
                                 Prefix Decrement -> ( pred w, pred w )
                                 Postfix Decrement -> ( w, pred w )
                        when ( store /= w' ) 
                             $ reject $ text "falscher Wert geschrieben" </> toDoc a
                        when ( Just result /= rvalue a ) 
                             $ reject $ text "falsches Resultat in rvalue" </> toDoc a
                    _ -> reject 
                        $ text "Präfix/Postfix-Operator erforder Read- und Write-Aktion" </> toDoc a
        
        
            ( Sequence , [ l, r ] ) -> do
                when ( lvalue r /= lvalue a )
                     $ reject $ text "lvalues von Wurzel und zweitem Argument müssen übereinstimmen" </> toDoc a
                when ( rvalue r /= rvalue a )
                     $ reject $ text "rvalues von Wurzel und zweitem Argument müssen übereinstimmen" </> toDoc a

            ( _ , [ l, r ] ) | Just f <- standard op -> do
                vl <- case rvalue l of
                    Nothing -> reject $ text "erstes Argument hat kein rvalue" </> toDoc a
                    Just vl -> return vl
                vr <- case rvalue r of
                    Nothing -> reject $ text "zweites Argument hat kein rvalue" </> toDoc a
                    Just vr -> return vr
                let result = f vl vr
                when ( rvalue a /= Just result ) 
                     $ reject $ text "falsches Resultat in rvalue" </> toDoc a

standard op = case op of
     Plus -> Just (+)
     Minus -> Just (-)
     Times -> Just (*)
     Divide -> Just div
     Remainder -> Just rem
     _ -> Nothing

-----------------------------------------------------

same_skeleton :: (Exp, Annotated) -> Reporter ()
same_skeleton (x, a) = case (x,node a) of
    (T.Symbol xs, Symbol ys) ->
        when ( xs /= ys ) 
             $ mismatch (text "verschiedene Bezeichner") x a
    (T.Literal i, Literal j) ->
        when ( i /= j ) 
             $ mismatch (text "verschiedene Literale") x a
    (Apply xop xargs, Operator cs ) -> do
        when ( oper xop /= cs) 
             $ mismatch (text "verschiedene Operatoren") x a
        when ( length xargs /= length ( children a )) 
             $ mismatch (text "verschiede Argumentanzahlen") x a
        forM_ ( zip xargs $ children a ) $ same_skeleton

mismatch msg x a = reject $ 
    msg </> vcat [ text "original" </> toDoc a
                 , text "annotiert" </> toDoc x 
                 ]

-------------------------------------------------------------------

