{-
this shows a feature of the Prelude's implementation of readList:
check will evaluate to error even if it's "obviously" the empty list
-}

data Foo = Foo

instance Read Foo where
    readsPrec = error "read"

check :: [ Foo ]
check = read "[]" 