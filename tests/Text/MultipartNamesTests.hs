{-# LANGUAGE NoMonomorphismRestriction #-}
module Text.MultipartNamesTests (tests) where

import Control.Lens
import Data.Char(toLower)
import Text.MultipartNames
import Test.Framework(Test)
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit(assertEqual)

src :: String
src = "better late than never"

nm :: MultipartName
nm = mkMultipartNameFromWords src

tests :: Test

    -- We have to play some odd games because Prism is a complex type
    -- and when we put it into a tuple (as in 'tab' below), it becomes
    -- impredicative and when we use it twice in the same function, we
    -- trip over monomorphism problems.	 (I don't fully understand
    -- it.)

tests = testCase "Text.MultipartNames" $ do
    mapM_ toName tab
    mapM_ toString tab

    where
    toName (tag, str, p, _cased) = do
	assertEqual (tag ++ " string to name") (Just nm) (preview p str)
    toString (tag, str, p, cased) = do
	assertEqual' cased (tag ++ " name to string") str (review p nm)

    assertEqual' cased msg expected actual = if cased
	then assertEqual msg expected actual
	else assertEqual msg (lower expected) (lower actual)
	where
	lower = map toLower

    tab = [
      ("lower-camel", "betterLateThanNever", _LowerCamel, True),
      ("upper-camel", "BetterLateThanNever", _UpperCamel, True),
      ("underscored", "better_Late_THAN_never", _Underscored, False),
      ("lower-underscored", "better_late_than_never", _LowerUnderscored, True),
      ("upper-underscored", "BETTER_LATE_THAN_NEVER", _UpperUnderscored, True),
      ("hyphenated", "Better-late-THAN-never", _Hyphenated, False),
      ("lower-hyphenated", "better-late-than-never", _LowerHyphenated, True),
      ("upper-hyphenated", "BETTER-LATE-THAN-NEVER", _UpperHyphenated, True)]
