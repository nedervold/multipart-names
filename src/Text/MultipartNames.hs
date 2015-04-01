-- | Multipart names and 'Prism's for them.
module Text.MultipartNames(
    -- * Construction
    mkMultipartName,
    mkMultipartNameFromWords,
    isLegalSegment,
    -- * Query
    toSegments,
    -- * Display
    -- ** camel-case
    showLowerCamel,
    showUpperCamel,
    -- ** hyphenated
    showLowerHyphenated,
    showUpperHyphenated,
    -- ** underscored
    showLowerUnderscored,
    showUpperUnderscored,
    -- * Prisms
    -- ** camel-case
    _LowerCamel,
    _UpperCamel,
    -- ** hyphenated
    _Hyphenated,
    _LowerHyphenated,
    _UpperHyphenated,
    -- ** underscored
    _Underscored,
    _LowerUnderscored,
    _UpperUnderscored
    ) where

import Control.Lens
import Text.MultipartNames.MultipartName
import Text.MultipartNames.Parsers
import Text.MultipartNames.Show

-- | A 'Prism'' to convert lowerCamelCased 'String's to
-- 'MultipartName's.
_LowerCamel :: Prism' String MultipartName
_LowerCamel = prism' showLowerCamel parseLowerCamel

-- | A 'Prism'' to convert UpperCamelCased 'String's to
-- 'MultipartName's.
_UpperCamel :: Prism' String MultipartName
_UpperCamel = prism' showUpperCamel parseUpperCamel

-- | A 'Prism'' to convert lower-hyphenated 'String's to
-- 'MultipartName's.
_LowerHyphenated :: Prism' String MultipartName
_LowerHyphenated = prism' showLowerHyphenated parseLowerHyphenated

-- | A 'Prism'' to convert UPPER-HYPHENATED 'String's to
-- 'MultipartName's.
_UpperHyphenated :: Prism' String MultipartName
_UpperHyphenated = prism' showUpperHyphenated parseUpperHyphenated

-- | A 'Prism'' to convert case-insensitive-hyphenated 'String's to
-- 'MultipartName's.
_Hyphenated :: Prism' String MultipartName
_Hyphenated = prism' showLowerHyphenated parseHyphenated

-- | A 'Prism'' to convert lower_underscored 'String's to
-- 'MultipartName's.
_LowerUnderscored :: Prism' String MultipartName
_LowerUnderscored = prism' showLowerUnderscored parseLowerUnderscored

-- | A 'Prism'' to convert UPPER_UNDERSCORED 'String's to
-- 'MultipartName's.
_UpperUnderscored :: Prism' String MultipartName
_UpperUnderscored = prism' showUpperUnderscored parseUpperUnderscored

-- | A 'Prism'' to convert Case_Insensitive_Underscored 'String's to
-- 'MultipartName's.
_Underscored :: Prism' String MultipartName
_Underscored = prism' showUpperUnderscored parseUnderscored

