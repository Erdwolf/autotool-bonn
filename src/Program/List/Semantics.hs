{-# language DeriveDataTypeable #-}

module Program.List.Semantics where

import Program.General.Environment as E
import Program.General.Program as P
import Program.List.Value as V
import Program.List.Expression as X
import Program.List.Operation as O
import Program.List.Store as S

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader

import Control.Monad ( forM )
import Control.Monad.State
import Data.Ix (inRange)
import Data.Typeable

execute ::  Environment V.Value
        -> Program Statement
        -> Reporter ( Environment V.Value )
execute env xs = evalStateT ( executeST env xs ) S.empty

data Statement = Statement Expression
    deriving Typeable
instance ToDoc Statement where 
    toDoc ( Statement x ) = toDoc x <> semi
instance Reader Statement where
    reader = do x <- reader ; my_semi ; return $ Statement x

executeST :: Environment V.Value
        -> Program Statement 
        -> ReporterST ( Environment V.Value )
executeST env0 ( Program xs ) = do
    env <- inject env0
    forM xs $ \ ( Statement x ) -> do 
        lift $ inform $ text "execute:" </> toDoc x
        eval env x
        env1 <- extract env
        lift $ inform $ text "Variablen:" </> toDoc env1
    env1 <- extract env
    return env1


inject :: Environment V.Value
       -> ReporterST ( Environment S.Key )
inject = E.mapM S.make 
extract :: Environment S.Key
       -> ReporterST ( Environment V.Value )
extract = E.mapM S.unmake


eval :: Environment S.Key
     -> Expression
     -> ReporterST S.Key
eval env x = case x of
    X.Scalar s -> S.scalar s
    X.Reference r -> do
        case E.lookup env r of
            Nothing -> lift $ reject $ hsep 
                       [ text "name", toDoc r, text "nicht gebunden" ]
            Just v  -> return v
    X.Methodcall self method args -> do
        k <- eval env self
        vself <- S.access k
        let matching = do
                  op <- O.ops
                  guard $ case S.typeof vself of
                      V.TCollect { V.name = n } -> n == O.object op
                      _ -> False
                  guard $ show method == O.method op
                  guard $ length args == length ( O.args op )
                  return op
        case matching of
            [    ] -> lift $ reject $ vcat
                  [ hsep [ text "Typ", toDoc ( S.typeof vself ) ]
                  , hsep [ text "besitzt keine Methode", toDoc method ]
                  , hsep [ text "mit", toDoc ( length args) , text "Argumenten" ]
                  ]
            [ op ] -> do
                  vargs <- sequence $ do
                      ( arg, ty ) <- zip args $ O.args op
                      return $ do
                          varg <- eval env arg
                          conform ty vself varg
                          return varg
                  O.semantics op k vargs

conform :: O.Type -> S.Contents -> S.Key -> ReporterST ()
conform ty vself varg = do
    val <- S.access varg
    case ( ty, S.typeof vself, val ) of
        ( Index, TCollect {}, S.Scalar {} ) -> 
            conform_range ( S.scontents val ) ( 0, length ( S.contents vself ) - 1 )
        ( Index', TCollect {}, S.Scalar {} ) -> 
            conform_range ( S.scontents val ) ( 0, length ( S.contents vself )  )
        ( Element, TCollect { V.arg = a }, _ ) | a == S.typeof val ->
            return ()
        _ -> do
            arg <- S.unmake varg
            lift $ reject $ vcat 
                [ hsep [ text "Argument", toDoc arg ]
                , hsep [ text "hat nicht den nötigen Typ", toDoc ty ]
                ]

conform_range i bnd = lift $  when ( not $ inRange bnd $ fromIntegral i ) 
    $ reject $ hsep 
             [ text "index", toDoc i
             , text "nicht im erlaubten Bereich" , toDoc bnd
             ]
    
