-- | Display of multipart names
module Text.MultipartNames.Show (
    showLowerCamel,
    showLowerHyphenated,
    showLowerUnderscored,
    showUpperCamel,
    showUpperHyphenated,
    showUpperUnderscored
    ) where

import qualified Data.CaseInsensitive as CI
import Data.Char
import Data.List(intercalate)
import Text.MultipartNames.MultipartName

-- | Display as lowerCamelCased.
showLowerCamel :: MultipartName -> String
showLowerCamel nm = concat (lower (CI.original $ head ss)
			: map (cap . CI.original) (tail ss))
    where
    ss = toSegments nm

-- | Display as UpperCamelCased.
showUpperCamel :: MultipartName -> String
showUpperCamel nm = concatMap (cap . CI.original) ss
    where
    ss = toSegments nm

-- | Display as lower-hyphenated.
showLowerHyphenated :: MultipartName -> String
showLowerHyphenated nm = intercalate "-" $ map (lower . CI.original) ss
    where
    ss = toSegments nm

-- | Display as UPPER-HYPHENATED.
showUpperHyphenated :: MultipartName -> String
showUpperHyphenated nm = intercalate "-" $ map (upper . CI.original) ss
    where
    ss = toSegments nm

-- | Display as lower_underscored.
showLowerUnderscored :: MultipartName -> String
showLowerUnderscored nm = intercalate "_" $ map (lower . CI.original) ss
    where
    ss = toSegments nm

-- | Display as UPPER_UNDERSCORED.
showUpperUnderscored :: MultipartName -> String
showUpperUnderscored nm = intercalate "_" $ map (upper . CI.original) ss
    where
    ss = toSegments nm

lower, upper, cap :: String -> String
lower = map toLower
upper = map toUpper
cap s = toUpper (head s) : lower (tail s)

