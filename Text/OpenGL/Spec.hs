{-# Language TypeSynonymInstances #-}
-- |
-- Code to represent and parse the enumext.spec file of the OpenGL
-- registry. It works on the revision: 11742 (dated Tue, 15 Jun 2010),
-- i.e. OpenGL 4.0. (The 4.1 specification appeared at the end of
-- July but the spec files are older.)
--
-- There is also some code to print the result back to something
-- close to the original representation, for checking purpose.
module Text.OpenGL.Spec (
  EnumLine(..), StartEnum(..), Value(..), Extension(..),
  enumLines, enumLine,
  parseAndShow, reparse,

  TmLine(..), TmType(..),
  tmLines, tmLine,

  FunLine(..), Field(..),
  funLines, funLine
  ) where

import Numeric (readHex, showHex)
import Data.Char (toUpper)
import Control.Applicative
import Text.ParserCombinators.Parsec hiding
  (many, optional, (<|>), token)


----------------------------------------------------------------------
--
-- Enumerants (enumext.spec)
--
----------------------------------------------------------------------

----------------------------------------------------------------------
-- Data structures (line oriented)
----------------------------------------------------------------------

-- Note : an interesting comment to recognize is Extension #xxx

-- | A complete representation of an enum.spec or enumext.spec line.
-- Each variant maps to one line of text. See 'enumLines' to parse
-- Strings to this representation.
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
  | Use String String
  -- ^ A use line.
  deriving (Eq, Show)

-- | The different ways to start an enumeration.
data StartEnum =
    Version Int Int Bool
  -- ^ Major, minor, the bool indicates if it is deprecated.
  | Extension Extension String Bool
  -- ^ The extension prefix, its, and whether it is deprecated.
  | Name String
  deriving (Eq, Show)

data Value = Hex Integer (Maybe HexSuffix) | Deci Int | String String
  deriving (Eq, Show)

data HexSuffix = U | Ull
  deriving (Eq, Show)

-- Note: what for FfdMaskSGIX? This will be a Name.
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
  deriving (Eq, Read, Show)

----------------------------------------------------------------------
-- Parsing (line oriented)
----------------------------------------------------------------------

-- | Parse a complete enumext.spec.
enumLines :: String -> Either ParseError [EnumLine]
enumLines = parse (many pEnumLine <* eof) "enumLines"

-- | Try to parse a line to its 'EnumLine' representation.
-- The '\n' character should be present at the end of the input.
enumLine :: String -> Either ParseError EnumLine
enumLine = parse pEnumLine "enumLine"

type P a = GenParser Char () a

pEnumLine :: P EnumLine
pEnumLine = choice
  [ try (Comment <$> pComment)
  , try (BlankLine <$ pBlankLine)
  , try pStart
  , try pPassthru
  , try pEnum
  , pUse
  ]

blanks :: P String
blanks = many (oneOf " \t")

blanks1 :: P String
blanks1 = many1 (oneOf " \t")

token :: String -> P String
token s = string s <* blanks

eol :: P ()
eol = () <$ char '\n'

digit' :: P Int
digit' = (read . (:[])) <$> digit

identifier :: P String
identifier = many1 . oneOf $ "_" ++ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

identifier_ :: P String
identifier_ = identifier <* blanks

value :: P Value
value = Hex . fst . head . readHex <$>
   try (string "0x" *> many1 hexDigit) <*>
   hexSuffix
  <|> Deci . read <$> many1 digit
  <|> String <$> identifier

opt :: String -> P Bool
opt s = maybe False (const True) <$> optional (string s)

hexSuffix :: P (Maybe HexSuffix)
hexSuffix = optional $ try (Ull <$ string "ull") <|> (U <$ string "u")

pComment :: P String
pComment = (\a b c -> concat [a,b,c]) <$>
  blanks <*> (string "#") <*> (many $ noneOf "\n")
  <* eol

pBlankLine :: P ()
pBlankLine = () <$ (blanks >> eol)

pStart :: P EnumLine
pStart = Start <$> pStartEnum <*>
  (blanks *> token "enum:" *> optional (many1 alphaNum)) <* eol

pPassthru :: P EnumLine
pPassthru = Passthru <$>
  (token "passthru:" *> token "/*"
  *> manyTill (noneOf "\n") (try $ string "*/")) <* eol

pEnum :: P EnumLine
pEnum = Enum <$>
  (blanks1 *> identifier_) <*>
  (char '=' *> blanks *> value) <*>
  (optional $ blanks *> char '#' *> blanks *> many1 (noneOf "\n")) <* eol

pUse :: P EnumLine
pUse = Use <$>
  (blanks1 *> token "use" *> identifier_) <*>
  identifier_ <* eol

pStartEnum :: P StartEnum
pStartEnum =
  Version <$>
  (string "VERSION_" *> digit') <*>
  (char '_' *> digit') <*>
  (opt "_DEPRECATED")
  <|>
  Extension <$> pExt <*> (char '_' *> identifier) <*>
  (opt "_DEPRECATED")
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

----------------------------------------------------------------------
-- Printing
-- This is mostly used as a sanity check by comparing the result against
-- the original input string. Some spaces and tabs, and some zero-padding
-- in hex numbers don't match. (The original format is aligned on
-- 8-column-wide tabstops.)
----------------------------------------------------------------------

-- | This function is useful to check the parse result. It parse a
-- enumext.spec file and try to print it back to the same format.
parseAndShow :: FilePath -> IO ()
parseAndShow fn = do
  c <- readFile fn
  case enumLines c of
    Left err -> putStrLn $ "Parse error: " ++ show err
    Right a -> putStrLn $ showEnumLines a

showEnumLines :: [EnumLine] -> String
showEnumLines = unlines . map showEnumLine

showEnumLine :: EnumLine -> String
showEnumLine el = case el of
  Comment x -> x
  BlankLine -> ""
  Start se Nothing -> showStartEnum se ++ " enum:" 
  Start se (Just x) -> showStartEnum se ++ " enum: " ++ x
  Passthru x -> "passthru: /* " ++ x ++ "*/"
  Enum a b Nothing -> "\t" ++ a ++ tabstop 55 a ++ "= " ++ showValue b
  Enum a b (Just x) -> "\t" ++ a ++ tabstop 55 a ++ "= " ++ showValue b ++ " # " ++ x
  Use a b -> "\tuse " ++ a ++ tabstop 39 (a ++ "    ") ++ "    " ++ b

tabstop :: Int -> String -> String
tabstop t a = replicate ((t - length a) `div` 8) '\t'

showStartEnum :: StartEnum -> String
showStartEnum se = case se of
  Version i j True -> "VERSION_" ++ show i ++ "_" ++ show j ++ "_DEPRECATED"
  Version i j False -> "VERSION_" ++ show i ++ "_" ++ show j
  Extension e x True -> showExtension e ++ "_" ++ x ++ "_DEPRECATED"
  Extension e x False -> showExtension e ++ "_" ++ x
  Name x -> x

showValue :: Value -> String
showValue v = case v of
  Hex i Nothing -> "0x" ++ showHex' i
  Hex i (Just U) -> "0x" ++ showHex' i ++ "u"
  Hex i (Just Ull) -> "0x" ++ showHex' i ++ "ull"
  Deci i -> show i
  String x -> x

showHex' :: Integral a => a -> String
showHex' i = replicate (4 - length h) '0' ++ h
  where h = map toUpper (showHex i "")

showExtension :: Extension -> String
showExtension e = case e of
  {-3-}DFX -> "3DFX"
       _ -> show e

----------------------------------------------------------------------
-- Sanity check
----------------------------------------------------------------------

-- | Parse a file, and check the result can be parsed again to the same
-- representation.
reparse :: FilePath -> IO ()
reparse fn = do
  c <- readFile fn
  case enumLines c of
    Left err -> putStrLn $
      "Error when parsing the original file: " ++ show err
    Right a -> case enumLines $ showEnumLines a of
      Left err -> putStrLn $
        "Error when parsing the printed result: " ++ show err
      Right b | a == b -> putStrLn "All's well that ends well."
              | otherwise -> putStrLn "Ouch, not good."

----------------------------------------------------------------------
--
-- Typemap (gl.tm)
--
----------------------------------------------------------------------

----------------------------------------------------------------------
-- Data structures (line oriented)
----------------------------------------------------------------------

data TmLine =
    TmComment String
  | TmEntry String TmType
  deriving (Eq, Show)

-- - The boolean is used for the presence or not of a *.
-- - The suffix Star is used when the * is always present.
data TmType =
    Star -- for void
  | GLbitfield
  | GLboolean Bool
  | GLbyte
  | GLchar Bool
  | GLcharARB Bool
  | GLclampd
  | GLclampf
  | GLdouble Bool
  | GLenum
--  | GLenumWithTrailingComma -- removed from the source
  | GLfloat Bool
  | UnderscoreGLfuncptr
  | GLhalfNV
  | GLhandleARB
  | GLint
  | GLint64
  | GLint64EXT
  | GLintptr
  | GLintptrARB
  | GLshort
  | GLsizei
  | GLsizeiptr
  | GLsizeiptrARB
  | GLsync
  | GLubyte
  | ConstGLubyteStar
  | GLuint
  | GLuint64
  | GLuint64EXT
  | GLUnurbsStar
  | GLUquadricStar
  | GLushort
  | GLUtesselatorStar
  | GLvoid Bool
  | GLvoidStarConst
  deriving (Eq, Read, Show)

----------------------------------------------------------------------
-- Parsing (line oriented)
----------------------------------------------------------------------

-- | Parse a complete gl.tm.
tmLines :: String -> Either ParseError [TmLine]
tmLines = parse (many pTmLine <* eof) "tmLines"

-- | Try to parse a line to its 'TMLine' representation.
-- The '\n' character should be present at the end of the input.
tmLine :: String -> Either ParseError TmLine
tmLine = parse pTmLine "tmLine"

pTmLine :: P TmLine
pTmLine = choice
  [ try (TmComment <$> pComment)
  , pTmEntry
  ]

pTmEntry :: P TmLine
pTmEntry = TmEntry <$>
  (identifier <* token ",*,*,") <*> pTmType
  <* (string ",*,*" >> opt ",") -- ignore trailing comma after GLenum line.
  <* eol

pTmType :: P TmType
pTmType = choice $ map try
  [ Star <$ string "*"
  , ConstGLubyteStar <$ string "const GLubyte *"
  , UnderscoreGLfuncptr <$ string "_GLfuncptr"
  , GLvoidStarConst <$ string "GLvoid* const"
  , GLboolean <$> (string "GLboolean" *> opt "*")
  , GLcharARB <$> (string "GLcharARB" *> opt "*")
  , GLchar <$> (string "GLchar" *> opt "*")
  , GLdouble <$> (string "GLdouble" *> opt "*")
  , GLfloat <$> (string "GLfloat" *> opt "*")
  , GLvoid <$> (string "GLvoid" *> opt "*")
  , GLUnurbsStar <$ string "GLUnurbs*"
  , GLUquadricStar <$ string "GLUquadric*"
  , GLUtesselatorStar <$ string "GLUtesselator*"
  , read <$> identifier
  ]

----------------------------------------------------------------------
-- Printing (TODO)
----------------------------------------------------------------------

----------------------------------------------------------------------
--
-- Functions (gl.spec)
--
----------------------------------------------------------------------

----------------------------------------------------------------------
-- Data structures (line oriented)
----------------------------------------------------------------------

data FunLine =
    FComment String
  | FBlankLine
  | Tag String [String]
  | FPassthru String
  | Function String [String]
  | Field Field
  | At String
  deriving (Eq, Show)

data Field =
    Return String
  | Param String String -- break into smaller parts
  | Category String String -- the last is a comment, should be Maybe
  | Subcategory String
  | FVersion Int Int
  | Glxropcode Glxropcode
  | Offset (Maybe Offset)
  | Wglflags [String]
  | Dlflags String
  | Glxflags [String] (Maybe String) -- The last is the comment
  | Glxsingle Glxsingle
  | Deprecated Int Int
  | Vectorequiv String
  | FExtension [String] -- verify if it should be Maybe String instead
  | Glxvendorpriv Glxvendorpriv
  | Glfflags [String]
  | Beginend [String] -- TODO Maybe instead of [] ?
  | Glxvectorequiv String -- TODO Bool ?
  | Alias String
  | Glextmask [String]
  deriving (Eq, Show)

data Glxropcode =
    Number Int (Maybe String)
  | QuestionMark
  deriving (Eq, Show)

data Glxsingle =
    Number2 Int
  | QuestionMark2
  deriving (Eq, Show)

data Offset =
    Number3 Int
  | QuestionMark3
  deriving (Eq, Show)

data Glxvendorpriv =
    Number4 Int
  | QuestionMark4
  deriving (Eq, Show)

----------------------------------------------------------------------
-- Parsing (line oriented)
----------------------------------------------------------------------

-- | Parse a complete gl.spec.
funLines :: String -> Either ParseError [FunLine]
funLines = parse (many pFunLine <* eof) "funLines"

-- | Try to parse a line to its 'TMLine' representation.
-- The '\n' character should be present at the end of the input.
funLine :: String -> Either ParseError FunLine
funLine = parse pFunLine "funLine"

tag :: P String
tag = many1 . oneOf $ "_-" ++ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']

tagValue :: P String
tagValue = (many1 . oneOf $ "_-*." ++ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'])
  <* blanks

field :: String -> P ()
field s = () <$ (blanks1 >> token s)

pFunLine :: P FunLine
pFunLine = choice
  [ try (FComment <$> pComment)
  , try (FBlankLine <$ pBlankLine)
  , try pFPassthru
  , try pTag
  , try pFunction
  , try pField
  , pAt
  ]

pFPassthru :: P FunLine
pFPassthru = FPassthru <$> (string "passthru:" *> many (noneOf "\n") <* eol)

pTag :: P FunLine
pTag = Tag <$> (tag <* char ':' <* blanks) <*> many tagValue <* eol

pFunction :: P FunLine
pFunction = Function <$>
  identifier <*> (char '(' *> sepBy identifier (token ",") <* char ')')
  <* eol

pField :: P FunLine
pField = Field <$> choice
  [ try pReturn
  , try pParam
  , try pCategory
  , try pVersion
  , try pGlxropcode
  , try pOffset
  , try pWglflags
  , try pDlflags
  , try pGlxflags
  , try pGlxsingle
  , try pDeprecated
  , try pVectorequiv
  , try pExtension
  , try pGlxvendorpriv
  , try pGlfflags
  , try pBeginend
  , try pGlxvectorequiv
  , try pAlias
  , try pSubcategory
  , try pGlextmask
  ]

pAt :: P FunLine
pAt = At <$> (token "@@@" *> many (noneOf "\n")) <* eol

pReturn :: P Field
pReturn = Return <$> (field "return" *> identifier) <* eol

pParam :: P Field
pParam = Param <$> (field "param" *> identifier_) <*> (many $ noneOf "\n") <* eol

pCategory :: P Field
pCategory = Category <$> (field "category" *> identifier_) <*> (many $ noneOf "\n") <* eol

pVersion :: P Field
pVersion = FVersion <$> (field "version" *> digit' <* char '.') <*> digit' <* eol

pGlxropcode :: P Field
pGlxropcode = Glxropcode <$> (field "glxropcode" *> pGlxropcode') <* eol

pGlxropcode' :: P Glxropcode
pGlxropcode' =
  Number <$> (read <$> many1 digit) <*> optional (identifier)
  <|> QuestionMark <$ string "?"

pOffset :: P Field
pOffset = Offset <$> (field "offset" *> optional pOffset') <* eol

pOffset' :: P Offset
pOffset' = 
  Number3 . read <$> many1 digit
  <|> QuestionMark3 <$ string "?"

pWglflags :: P Field
pWglflags = Wglflags <$> (field "wglflags" *> many (tag <* blanks)) <* eol

pDlflags :: P Field
pDlflags = Dlflags <$> (field "dlflags" *> tag) <* eol

pGlxflags :: P Field
pGlxflags = Glxflags <$>
  (field "glxflags" *> many (tag <* blanks)) <*>
  optional (many $ noneOf "\n")
  <* eol

pGlxsingle :: P Field
pGlxsingle = Glxsingle <$> (field "glxsingle" *> pGlxsingle') <* eol

pGlxsingle' :: P Glxsingle
pGlxsingle' = 
  Number2 . read <$> many1 digit
  <|> QuestionMark2 <$ string "?"

pDeprecated :: P Field
pDeprecated = Deprecated <$>
  (field "deprecated" *> digit' <* char '.') <*> digit' <* eol

pVectorequiv :: P Field
pVectorequiv = Vectorequiv <$> (field "vectorequiv" *> identifier_) <* eol

pExtension :: P Field
pExtension =  FExtension <$> (field "extension" *> many identifier_) <* eol

pGlxvendorpriv :: P Field
pGlxvendorpriv =
  Glxvendorpriv <$> (field "glxvendorpriv" *> pGlxvendorpriv') <* eol

pGlxvendorpriv' :: P Glxvendorpriv
pGlxvendorpriv' = 
  Number4 . read <$> many1 digit
  <|> QuestionMark4 <$ string "?"

-- TODO Maybe instead of [] ?
pGlfflags :: P Field
pGlfflags = Glfflags <$> (field "glfflags" *> many (tag <* blanks)) <* eol

pBeginend :: P Field
pBeginend = Beginend <$> (field "beginend" *> many (tag <* blanks)) <* eol

pGlxvectorequiv :: P Field
pGlxvectorequiv =
  Glxvectorequiv <$> (field "glxvectorequiv" *> identifier_) <* eol

pAlias :: P Field
pAlias = Alias <$> (field "alias" *> identifier_) <* eol

pSubcategory :: P Field
pSubcategory = Subcategory <$> (field "subcategory" *> identifier_) <* eol

pGlextmask :: P Field
pGlextmask =
  Glextmask <$> (field "glextmask" *> sepBy identifier (token "|")) <* eol

