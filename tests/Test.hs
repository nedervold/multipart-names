module Main where

import Text.MultipartNamesTests(tests)
import Test.Framework(defaultMain)

main :: IO ()
main = defaultMain [tests]


