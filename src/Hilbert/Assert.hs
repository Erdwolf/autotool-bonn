module Assert

( assert
)

where

assert :: MonadZero m => Bool -> m ()
assert b = if b then return () else zero


