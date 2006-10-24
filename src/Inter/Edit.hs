module Inter.Edit where

import Inter.CGI

class Edit a where
    edit :: Monad m => a -> Form m a

