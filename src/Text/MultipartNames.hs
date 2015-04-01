-- | Multipart names and 'Prism's for them.
module Text.MultipartNames(
    -- * Creation
    module Text.MultipartNames.MultipartName,
    -- * Display
    module Text.MultipartNames.Show,
    -- * Prisms
    _LowerCamel,
    _LowerUnderscored,
    _Underscored,
    _UpperCamel,
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

-- | A 'Prism'' to convert lower_underscored 'String's to
-- 'MultipartName's.
_LowerUnderscored :: Prism' String MultipartName
_LowerUnderscored = prism' showLowerUnderscored parseLowerUnderscored

-- | A 'Prism'' to convert UPPER_UNDERSCORED 'String's to
-- 'MultipartName's.
_UpperUnderscored :: Prism' String MultipartName
_UpperUnderscored = prism' showUpperUnderscored parseUpperUnderscored

-- | A 'Prism'' to convert Case_Insensitively_Underscored 'String's to
-- 'MultipartName's.
_Underscored :: Prism' String MultipartName
_Underscored = prism' showUpperUnderscored parseUnderscored

