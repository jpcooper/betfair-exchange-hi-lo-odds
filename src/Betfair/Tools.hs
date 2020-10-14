module Betfair.Tools (assert) where

import Prelude (Bool, String, error)

assert :: Bool -> String -> a -> a
assert assertion messageIfFails value =
  if assertion
  then value
  else error messageIfFails
