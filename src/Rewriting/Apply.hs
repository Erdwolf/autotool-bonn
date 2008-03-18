module Rewriting.Apply where

import Autolib.Reporter
import Autolib.ToDoc



class Apply action object where
    apply :: action -> oobject -> Reporter object

class Actions system object action where
    actions :: system -> object -> [ action ]

 
    

