-- | Multipart names.
module Text.MultipartNames where

-- import Control.Lens
import Data.CaseInsensitive(CI)
import qualified Data.CaseInsensitive as CI
import Data.Char(isAscii, isLetter)

-- | An opaque type that represents a multipart name.  The initial
-- character of each segment must be a cased letter.  For now, we only
-- allow ASCII.	 In the future, we may expand to Unicode.
newtype MultipartName = MultipartName [CI String]
    deriving (Eq, Ord)

{-
instance Show MultipartName where
    show = show .
-}

-- | Create a multipart name from its segments.
mkMultipartName :: [String] -> MultipartName
mkMultipartName ss
    | null ss		= error "mkMultipartName []: argument cannot be null"
    | all legalSegment ss = MultipartName $ map CI.mk ss
    | otherwise		= error msg
    where
    legalSegment seg = case seg of
	[] -> False
	c : _ -> isAscii c && isLetter c
    msg = "mkMultipartName " ++ show ss
			     ++ ": all segments must start with a cased letter"

mkMultipartNameFromWords :: String -> MultipartName
mkMultipartNameFromWords = mkMultipartName . words
