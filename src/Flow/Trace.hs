module Flow.Trace where

import Flow.State
import Flow.Action

import Autolib.ToDoc
import Autolib.Util.Size

data List a b = Cons a ( List b a ) | Nil
     deriving ( Eq, Ord )

top ( Cons a b ) = a

instance Size ( List a b ) where
    size Nil = 0
    size (Cons a b ) = succ ( size b )

instance ( ToDoc a, ToDoc b ) 
        => ToDoc ( List a b ) where
    toDoc l = dutch_list $ h l

-- strange: this signature is needed
h :: ( ToDoc a, ToDoc b )   => List a b -> [Doc]
h Nil = []
h (Cons x y) = toDoc x : h y

data Trace = Trace { contents :: List State Action
                   , complete :: Bool
                   }
    deriving ( Eq, Ord )

instance Size Trace where size = size . contents

instance ToDoc Trace where
    toDoc t = named_dutch_record "Trace" 
            [ text "contents" <+> equals <+> vcat ( reverse $ h $ contents t )
            , text "complete" <+> equals <+> toDoc ( complete t )
            ]

start s = Trace { contents = Cons s Nil
                , complete = True 
                }

current :: Trace -> State
current t = top ( contents t )

extend n a t | complete t = 
    t { contents = Cons n $ Cons a $ contents t
      , complete = True
      }
