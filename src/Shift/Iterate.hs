module Shift.Iterate where


iterate_strict          :: (a -> a) -> a -> [a]
iterate_strict f x       = x `seq` x : iterate_strict f (f x)

