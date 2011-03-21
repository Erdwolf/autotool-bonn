module Type.Lookup where

import Type.Data
import Autolib.Reporter
import Autolib.ToDoc
import Autolib.TES.Identifier

look_here :: Signature 
       -> Identifier
       -> [ Tag ]
look_here sig x = 
       do c <- classes sig ; guard $ cname c == x ; return $ Class_Tag c
    ++ do m <- methods sig ; guard $ mname m == x ; return $ Method_Tag m
    ++ do v <- variables sig ; guard $ vname v == x ; return $ Variable_Tag v

look_deep :: Signature 
       -> Signature
       -> Qualified_Name 
       -> Reporter [ Tag ]
look_deep sig0 sig ( Qualified_Name [] ) = do
    reject $ text "seltsamer Name []"
look_deep sig0 sig ( Qualified_Name ( x : xs ) ) = do
    let tags = look_here sig x
    if  null xs
        then return tags
        else do
            yss <- sequence $ do
                 Class_Tag c <- tags
                 return $ do
                    tags <- look_deep sig0 ( csignature c ) ( Qualified_Name xs )
                    return $ filter is_static tags
            zss <- sequence $ do
                 Variable_Tag v <- tags
                 return $ do
                     let Type t = vtype v
                     ctags <- look_deep sig0 sig0 t
                     tags <- sequence $ do
                         Class_Tag c <- ctags
                         return $ do
                             tags <- look_deep sig0 ( csignature c ) 
                                 ( Qualified_Name xs )
                             return tags
                     return $ concat tags
            return $ concat $ yss ++ zss

is_static ( Method_Tag m ) = mstatic m
is_static ( Variable_Tag v ) = vstatic v
is_static _                  = False