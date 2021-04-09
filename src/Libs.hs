-- Utils
-- Powered by BlackCloud

module Libs 
(
  check
) where

assert :: Bool -> String 
assert False = error "assertion failed!"
assert _     = "check passed"

check :: Bool -> IO()
check b = putStrLn (assert b)