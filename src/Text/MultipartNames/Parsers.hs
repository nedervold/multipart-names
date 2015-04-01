-- | Parsers for strings in various formats.
module Text.MultipartNames.Parsers(
    parseLowerCamel,
    parseLowerUnderscored,
    parseUnderscored,
    parseUpperCamel,
    parseUpperUnderscored
    ) where

import Data.Char
import Text.MultipartNames.MultipartName
import Text.ParserCombinators.Parsec hiding (lower, upper)

type P = GenParser Char ()

-- Names

-- | Parse a lowerCamelCased 'String'.
parseLowerCamel :: String -> Maybe MultipartName
parseLowerCamel str = case parse pLowerCamel undefined str of
			  Left _ -> Nothing
			  Right nm -> Just nm
    where
    pLowerCamel = do
		      ls <- pLowerSegment
		      uss <- many pUpperCamelSegment
		      eof
		      return $ mkMultipartName (ls : uss)

-- | Parse a UpperCamelCased 'String'.
parseUpperCamel :: String -> Maybe MultipartName
parseUpperCamel str = case parse pUpperCamel undefined str of
			  Left _ -> Nothing
			  Right nm -> Just nm
    where
    pUpperCamel = do
		      uss <- many1 pUpperCamelSegment
		      eof
		      return $ mkMultipartName uss

-- | Parse a lower_underscored 'String'.
parseLowerUnderscored :: String -> Maybe MultipartName
parseLowerUnderscored str = case parse pLowerUnderscored undefined str of
			  Left _ -> Nothing
			  Right nm -> Just nm
    where
    pLowerUnderscored = do
		      ls <- pLowerSegment
		      uss <- many pLowerUnderscoredSegment
		      eof
		      return $ mkMultipartName (ls : uss)

-- | Parse a Case_Insensitively_Underscored 'String'.
parseUnderscored :: String -> Maybe MultipartName
parseUnderscored str = case parse pUnderscored undefined str of
			  Left _ -> Nothing
			  Right nm -> Just nm
    where
    pUnderscored = do
		      ls <- pSegment
		      uss <- many pUnderscoredSegment
		      eof
		      return $ mkMultipartName (ls : uss)

-- | Parse a UPPER_UNDERSCORED 'String'.
parseUpperUnderscored :: String -> Maybe MultipartName
parseUpperUnderscored str = case parse pUpperUnderscored undefined str of
			  Left _ -> Nothing
			  Right nm -> Just nm
    where
    pUpperUnderscored = do
		      ls <- pUpperSegment
		      uss <- many pUpperUnderscoredSegment
		      eof
		      return $ mkMultipartName (ls : uss)

-- Segments

pLowerSegment :: P String
pLowerSegment = do
		    c <- satisfy isAsciiLower
		    cs <- many (satisfy isAsciiLowerOrNum)
		    return (c : cs)

pUpperSegment :: P String
pUpperSegment = do
		    c <- satisfy isAsciiUpper
		    cs <- many (satisfy isAsciiUpperOrNum)
		    return (c : cs)

pSegment :: P String
pSegment = do
		    c <- satisfy isAsciiAlpha
		    cs <- many (satisfy isAsciiAlphaNum)
		    return (c : cs)

pUpperCamelSegment :: P String
pUpperCamelSegment = do
		    c <- satisfy isAsciiUpper
		    cs <- many (satisfy isAsciiLowerOrNum)
		    return (c : cs)

pLowerUnderscoredSegment :: P String
pLowerUnderscoredSegment = char '_' >> pLowerSegment

pUnderscoredSegment :: P String
pUnderscoredSegment = char '_' >> pSegment

pUpperUnderscoredSegment :: P String
pUpperUnderscoredSegment = char '_' >> pUpperSegment

-- Predicates

isAsciiLowerOrNum :: Char -> Bool
isAsciiLowerOrNum c = isAscii c && (isLower c || isDigit c)

isAsciiUpperOrNum :: Char -> Bool
isAsciiUpperOrNum c = isAscii c && (isUpper c || isDigit c)

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAscii c && isAlphaNum c

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = isAscii c && isAlpha c

