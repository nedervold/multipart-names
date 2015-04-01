-- | Multipart names.
module Text.MultipartNames.MultipartName(
    MultipartName,
    mkMultipartName,
    mkMultipartNameFromWords,
    isLegalSegment,
    toSegments
    ) where

-- import Control.Lens
import Data.CaseInsensitive(CI)
import qualified Data.CaseInsensitive as CI
import Data.Char(isAscii, isLetter)

-- | An opaque type that represents a multipart name.  The initial
-- character of each segment must be a cased letter.  For now, we only
-- allow ASCII.	 In the future, we may expand to Unicode.
newtype MultipartName = MultipartName {
    toSegments :: [CI String]	 -- ^ Returns the segments of the name
    }
    deriving (Eq, Ord)

-- | Creates a multipart name from its segments.
mkMultipartName :: [String] -> MultipartName
mkMultipartName ss
    | null ss		= error "mkMultipartName []: argument cannot be null"
    | all isLegalSegment ss
	  = MultipartName $ map CI.mk ss
    | otherwise		= error msg
    where
    msg = "mkMultipartName " ++ show ss
			     ++ ": all segments must start with a cased letter"


-- | Creates a multipart name from words.  Equivalent to
-- @mkMultipartName . words@.
mkMultipartNameFromWords :: String -> MultipartName
mkMultipartNameFromWords = mkMultipartName . words

-- | Is this string a legal segment for a 'MultipartName'?
isLegalSegment :: String -> Bool
isLegalSegment seg = case seg of
    [] -> False
    c : _ -> isAscii c && isLetter c
