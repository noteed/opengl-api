{-# Language TypeSynonymInstances #-}
-- |
-- Code to represent and parse the enumext.spec file of the OpenGL
-- registry. It works on the revision: 11742 (dated Tue, 15 Jun 2010),
-- i.e. OpenGL 4.1.
module Text.OpenGL.Spec where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

----------------------------------------------------------------------
-- Data structures (line oriented)
----------------------------------------------------------------------

-- Note : an interesting comment to recognize is Extension #xxx

-- | A complete representation of an enum.spec or enumext.spec line.
-- Each variant maps to one line of text.
data EnumLine =
    Comment String
  -- ^ A comment on its own line, beginning with #.
  | BlankLine
  -- ^ A single blanck line.
  | Start StartEnum (Maybe String)
  -- ^ The beginning of an enumeration.
  | Passthru String
  -- ^ A passthru line with its comment.
  | Enum String Value (Maybe String)
  -- ^ An enumerant, in format String = String # String.
  | Use String String -- ^ A use line.
  deriving Show

-- | The different ways to start an enumeration.
data StartEnum =
    Version Int Int Bool -- (Maybe String)
  -- ^ Major, minor, the bool indicates if it is deprecated.
  -- The Maybe String is in Start
  | Extension Extension String Bool
  -- ^ The extension prefix, its, and whether it is deprecated.
  | Name String
  deriving Show

-- Hex 'xxx' optional trailing 'u' or 'ull'
data Value = Hex String (Maybe String) | Deci String | String String
  deriving Show

-- Note: what for FfdMaskSGIX?
-- | The different kinds of extension used to start an enumeration.
data Extension =
  {-3-}DFX
  | AMD
  | APPLE
  | ARB
  | ATI
  | EXT
  | GREMEDY
  | HP
  | IBM
  | INGR
  | INTEL
  | MESA
  | MESAX
  | NV
  | OES
  | OML
  | PGI
  | REND
  | S3
  | SGI
  | SGIS
  | SGIX
  | SUN
  | SUNX
  | WIN
  deriving (Read, Show)

----------------------------------------------------------------------
-- Parsing (line oriented)
----------------------------------------------------------------------

-- | Return the list of the unparsed lines, with their line number.
enumLines :: [String] -> ([(Int,String)],[EnumLine])
enumLines x = go 1 x [] []
  where
  go _ [] failure success = (reverse failure, reverse success)
  go i (l:ls) failure success = case enumLine l of
    Nothing -> go (i+1) ls ((i,l):failure) success
    Just s -> go (i+1) ls failure (s:success)

enumLine :: String -> Maybe EnumLine
enumLine x = case enumLine' x of
  Left _ -> Nothing
  Right a -> Just a

enumLine' :: String -> Either ParseError EnumLine
enumLine' = parse pEnumLine "enumLine"

type P a = GenParser Char () a

pEnumLine :: P EnumLine
pEnumLine = choice
  [ try pComment
  , try pBlankLine
  , try pStart
  , try pPassthru
  , try pEnum
  , pUse
  ]

blanks :: P ()
blanks = () <$ many (oneOf " \t")

blanks1 :: P ()
blanks1 = () <$ many1 (oneOf " \t")

eol :: P ()
eol = () <$ char '\n' <|> eof

digit' :: P Int
digit' = (read . (:[])) <$> digit

-- TODO factor the xxxString (maybe take care of the trailing blanks),
-- maybe disallow beginning by a number.

enumString :: P String
enumString = (many1 . oneOf $ "_x" ++ ['0'..'9'] ++ ['A'..'Z']) <* blanks

useString :: P String
useString = (many1 . oneOf $ "_" ++ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']) <* blanks

extString :: P String
extString = (many1 . oneOf $ "_" ++ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'])

-- TODO actual hexa and deci value
value :: P Value
value = Hex <$> try (string "0x" *> many1 hexDigit) <*>
   (optional (try (string "ull") <|> string "u"))
  <|> Deci <$> many1 digit
  <|> String <$> extString

pComment :: P EnumLine
pComment = Comment <$> (blanks *> char '#' *> many (noneOf "\n")) <* eol

pBlankLine :: P EnumLine
pBlankLine = BlankLine <$ (blanks >> eol)

pStart :: P EnumLine
pStart = Start <$> pStartEnum <*>
  (blanks *> string "enum:" *> blanks *> optional (many1 alphaNum)) <* eol

pPassthru :: P EnumLine
pPassthru = Passthru <$>
  (string "passthru:" *> blanks *> string "/*"
  *> blanks *> manyTill (noneOf "\n") (try $ string "*/")) <* eol

pEnum :: P EnumLine
pEnum = Enum <$>
  (blanks1 *> enumString) <*>
  (char '=' *> blanks *> value) <*>
  (optional $ blanks *> char '#' *> blanks *> many1 (noneOf "\n")) <* eol

pUse :: P EnumLine
pUse = Use <$>
  (blanks1 *> string "use" *> blanks *> useString) <*>
  (blanks *> enumString) <* eol

pStartEnum :: P StartEnum
pStartEnum =
  Version <$>
  (string "VERSION_" *> digit') <*>
  (char '_' *> digit') <*>
  (maybe False (const True) <$> optional (string "_DEPRECATED"))
  <|>
  Extension <$> pExt <*> (char '_' *> extString) <*>
  (maybe False (const True) <$> optional (string "_DEPRECATED"))
  <|>
  Name <$> many alphaNum

pExt :: P Extension
pExt = choice $ map (fmap r . try . string)
  [ "3DFX"
  , "AMD"
  , "APPLE"
  , "ARB"
  , "ATI"
  , "EXT"
  , "GREMEDY"
  , "HP"
  , "IBM"
  , "INGR"
  , "INTEL"
  , "MESAX"
  , "MESA"
  , "NV"
  , "OES"
  , "OML"
  , "PGI"
  , "REND"
  , "S3"
  , "SGIS"
  , "SGIX"
  , "SGI"
  , "SUNX"
  , "SUN"
  , "WIN"
  ]
  where r "3DFX" = {-3-}DFX
        r x = read x

