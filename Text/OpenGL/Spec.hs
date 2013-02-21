-- |
-- Code to represent and parse the enumext.spec file of the OpenGL
-- registry. It works on the revision: 11742 (dated Tue, 15 Jun 2010),
-- i.e. OpenGL 4.0. (The 4.1 specification appeared at the end of
-- July but the spec files are older.)
--
-- There is also some code to print the result back to something
-- close to the original representation, for checking purpose.
module Text.OpenGL.Spec (
  EnumLine(..), Category(..), Value(..), Extension(..), Profile(..),
  enumLines, enumLine,
  parseAndShow, reparse,
  showCategory, pCategory,

  TmLine(..), TmType(..),
  tmLines, tmLine,

  FunLine(..), Prop(..), ReturnType(..), ParamType(..), Passing(..),
  Question(..), Wglflag(..), Dlflag(..), Glxflag(..),
  FExtension(..), Glfflag(..),
  funLines, funLine,

  showExtension,
  HexSuffix(..),

  isReturn,
  isParam,
  isCategory,
  isSubcategory,
  isProfile,
  isFVersion,
  isGlxropcode,
  isOffset,
  isWglflags,
  isDlflags,
  isGlxflags,
  isGlxsingle,
  isDeprecated,
  isFExtension,
  isGlxvendorpriv,
  isGlfflags,
  isAllowInside,
  isVectorequiv,
  isGlxvectorequiv,
  isAlias,
  isGlextmask
  ) where

import Numeric (readHex, showHex)
import Data.Char (toUpper)
import Control.Applicative
import Text.Parsec hiding
    (many, optional, (<|>), token)
import Text.Parsec.String

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
  | Start Category (Maybe String)
  -- ^ The beginning of an enumeration.
  | Profile Profile
  -- ^ The signal for a new profile
  | Passthru String
  -- ^ A passthru line with its comment.
  | Enum String Value (Maybe String)
  -- ^ An enumerant, in format String = String # String.
  | Use Category String
  -- ^ A use line.
  deriving (Eq, Show)

-- | The different ways to start an enumeration.

data Category =
    Version Int Int Bool
  -- ^ Major, minor, the bool indicates if it is deprecated.
  | Extension Extension String Bool
  -- ^ The extension prefix, its, and whether it is deprecated.
  | Name String
  deriving (Eq, Ord, Show)

data Value = Hex Integer Int (Maybe HexSuffix) | Deci Int | Identifier String
  deriving (Eq, Show)

data HexSuffix = U | Ull
  deriving (Eq, Show)

data Profile
    = Compatibility
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
  | KHR
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
  deriving (Eq, Ord, Read, Show)

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

type P = Parser

pEnumLine :: P EnumLine
pEnumLine = choice
  [ try (Comment <$> pComment)
  , try (BlankLine <$ pBlankLine)
  , try pStart
  , try pPassthru
  , try pEnum
  , try pProfile
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

depIdentifier :: P String
depIdentifier = many1 ( notFollowedBy (string "_DEPRECATED") *> oneOf idents)
    where idents = "_" ++ ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']


identifier_ :: P String
identifier_ = identifier <* blanks

value :: P Value
value = pHex
  <|> Deci . read <$> (optionMaybe (char '-') >>= \sig ->
             (maybe (id) (:) sig) <$> many1 digit)
  <|> Deci . read <$> (maybe id (:) <$> optionMaybe (char '-') <*> many1 digit)
  <|> Identifier <$> identifier

pHex :: P Value
pHex = h <$>
   try (string "0x" *> many1 hexDigit) <*>
   hexSuffix
  where h s = Hex (fst . head $ readHex s) (length s)

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
pStart = Start <$> pCategory <*>
  (blanks *> token "enum:" *> optional (many1 $ noneOf "\n")) <* eol

pPassthru :: P EnumLine
pPassthru = Passthru <$>
  (token "passthru:" *> token "/*"
  *> manyTill (noneOf "\n") (try $ string "*/")) <* eol

pEnum :: P EnumLine
pEnum = Enum <$>
  (blanks1 *> identifier_) <*>
  (char '=' *> blanks *> value) <*>
  (optional $ blanks *> char '#' *> blanks *> many1 (noneOf "\n")) <* eol

pProfile :: P EnumLine
pProfile = Profile <$>
    (token "profile:" *> blanks *> pProfileVal <* eol)

pProfileVal :: P Profile
pProfileVal =
    Compatibility <$ string "compatibility"

pUse :: P EnumLine
pUse = Use <$>
  (blanks1 *> token "use" *> pCategory <* blanks1) <*>
  identifier_ <* optional (blanks *> string "#" *> (many $ noneOf "\n")) <* eol

pCategory :: P Category
pCategory =
  Version <$>
  try (string "VERSION_" *> digit') <*>
  (char '_' *> digit') <*>
  (opt "_DEPRECATED")
  <|>
  Extension <$> pExt <*> (char '_' *> depIdentifier) <*>
  (opt "_DEPRECATED")
  <|>
  Name <$> tag -- many1 alphaNum
  -- alphaNum is enough for enumext.spec;
  -- tag is used for the CategoryProp.

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
  , "KHR"
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
  Start se Nothing -> showCategory se ++ " enum:"
  Start se (Just x) -> showCategory se ++ " enum: " ++ x
  Passthru x -> "passthru: /* " ++ x ++ "*/"
  Enum a b Nothing -> "\t" ++ a ++ tabstop 55 a ++ "= " ++ showValue b
  Enum a b (Just x) -> "\t" ++ a ++ tabstop 55 a ++ "= " ++ showValue b ++ " # " ++ x
  Profile p -> "profile: " ++ showProfile p
  Use a b -> "\tuse " ++ showCategory a ++ tabstop 39 (showCategory a ++ "    ") ++ "    " ++ b

showProfile :: Profile -> String
showProfile p = case p of
    Compatibility -> "compatibility"

tabstop :: Int -> String -> String
tabstop t a = replicate ((t - length a) `div` 8) '\t'

showCategory :: Category -> String
showCategory se = case se of
  Version i j True -> "VERSION_" ++ show i ++ "_" ++ show j ++ "_DEPRECATED"
  Version i j False -> "VERSION_" ++ show i ++ "_" ++ show j
  Extension e x True -> showExtension e ++ "_" ++ x ++ "_DEPRECATED"
  Extension e x False -> showExtension e ++ "_" ++ x
  Name x -> x

showValue :: Value -> String
showValue v = case v of
  Hex i l Nothing -> "0x" ++ showHex' l i
  Hex i l (Just U) -> "0x" ++ showHex' l i ++ "u"
  Hex i l (Just Ull) -> "0x" ++ showHex' l i ++ "ull"
  Deci i -> show i
  Identifier x -> x

showHex' :: (Show a, Integral a) => Int -> a -> String
showHex' l i = replicate (l - length h) '0' ++ h
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
  | TmEntry String TmType Bool
  -- The boolean is used for the presence or not of a *.
  deriving (Eq, Show)

data TmType =
    Star -- for void
  | GLbitfield
  | GLboolean
  | GLbyte
  | GLchar
  | GLcharARB
  | GLcharStarConst
  | GLclampd
  | GLclampf
  | GLdouble
  | GLenum
--  | GLenumWithTrailingComma -- removed from the source
  | GLfloat
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
  | ConstGLubyte
  | GLuint
  | GLuint64
  | GLuint64EXT
  | GLUnurbs
  | GLUquadric
  | GLushort
  | GLUtesselator
  | GLvoid
  | GLvoidStarConst
  | GLvdpauSurfaceNV
  | GLdebugproc
  | GLdebugprocAMD
  | GLdebugprocARB
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
  (identifier <* token ",*,*,") <*> pTmType <*> opt "*"
  <* (string ",*,*" >> opt ",") -- ignore trailing comma after GLenum line, and sync.
  <* eol

pTmType :: P TmType
pTmType = choice $ map try
  [ Star <$ string "*"
  , ConstGLubyte <$ token "const GLubyte"
  , UnderscoreGLfuncptr <$ token "_GLfuncptr"
  , GLvoidStarConst <$ token "GLvoid* const"
  , GLcharStarConst <$ token "GLchar* const"
  , GLboolean <$ token "GLboolean"
  , GLcharARB <$ token "GLcharARB"
  , GLchar <$ token "GLchar"
  , GLdouble <$ token "GLdouble"
  , GLfloat <$ token "GLfloat"
  , GLvoid <$ token "GLvoid"
  , GLUnurbs <$ token "GLUnurbs"
  , GLUquadric <$ token "GLUquadric"
  , GLUtesselator <$ token "GLUtesselator"
  , GLvdpauSurfaceNV <$ token "GLvdpauSurfaceNV"
  , GLdebugprocAMD <$ token "GLDEBUGPROCAMD"
  , GLdebugprocARB <$ token "GLDEBUGPROCARB"
  , GLdebugproc    <$ token "GLDEBUGPROC"
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
  | FPassthru String
  | Property Property
  | NewCategory Category
  | Function String [String]
  | Prop Prop
  | At String
  deriving (Eq, Show)

data Property =
    RequiredProps
  -- ^ Hardcoded counter part (empty)
  | ParamProp
  -- ^ Hardcoded counter part: retval retained, retval is not used
  | DlflagsProp
  -- ^ Hardcoded counter part: notlistable handcode
  | GlxflagsProp
  -- ^ Hardcoded counter part:
  -- client-intercept client-handcode server-handcode EXT SGI ignore ARB
  | VectorequivProp
  -- ^ Hardcoded counter part: *
  | CategoryProp [Category]
  -- ^ Could have been hardcoded too, but there are many values.
  | VersionProp [(Int,Int)]
  -- ^ Could have been hardcoded too.
  | DeprecatedProp [(Int,Int)]
  -- ^ Could have been hardcoded too. Only 3.1 for now.
  | ProfileProp [Profile]
  -- ^ only compatibility for now.
  | GlxsingleProp
  -- ^ Hardcoded counter part: *
  | GlxropcodeProp
  -- ^ Hardcoded counter part: *
  | GlxvendorprivProp
  -- ^ Hardcoded counter part: *
  | WglflagsProp
  -- ^ Hardcoded counter part:
  -- client-handcode server-handcode small-data batchable
  | ExtensionProp
  -- ^ Hardcoded counter part:
  -- future not_implemented soft WINSOFT NV10 NV20 NV50
  -- future and not_implemented are unused.
  | AliasProp
  -- ^ Hardcoded counter part: *
  | OffsetProp
  -- ^ Hardcoded counter part: *
  | GlfflagsProp
  -- ^ Hardcoded counter part: *
  | BeginendProp
  -- ^ Hardcoded counter part: *
  | GlxvectorequivProp
  -- ^ Hardcoded counter part: *
  | SubcategoryProp
  -- ^ Hardcoded counter part: *
  | GlextmaskProp
  -- ^ Hardcoded counter part: *
  deriving (Eq, Show)

data Prop =
    Return ReturnType
  | Param String ParamType
  -- ^ This pairs the name of a parameter with its type.
  | Category Category (Maybe Category)
   -- ^ The Maybe is a commented old value.
  | FProfile Profile
  | Subcategory String
  | FVersion Int Int
  | Glxropcode Question
  | Offset (Maybe Question)
  | Wglflags [Wglflag]
  | Dlflags Dlflag
  | Glxflags [Glxflag] (Maybe [Glxflag])
  -- ^ The first list contains the actuals flags while the second list
  -- contains commented flags. The Maybe wrapper could be removed.
  | Glxsingle Question
  | Deprecated Int Int
  -- ^ Only 3.1 for now.
  | FExtension [FExtension]
  | Glxvendorpriv Question
  | Glfflags [Glfflag]
  | AllowInside
  -- ^ The beginend property has always the value allow-inside, so instead of
  -- a constructor Beginend String, this property works like a flag.
  | Vectorequiv String
  -- ^ See the following field's description.
  | Glxvectorequiv String
  -- ^ This could be a single flag: the equivalent name is derived from the
  -- name of the function. It seems this flag is present exactly when
  -- Vectorequiv is present.
  | Alias String
  | Glextmask [String]
  deriving (Eq, Show)

data ReturnType =
    Boolean
  | BufferOffset
  | ErrorCode
  | Float32
  | FramebufferStatus
  | GLEnum
  | HandleARB
  | Int32
  | List
  | Path
  | String
  | Sync
  | UInt32
  | UInt64
  | Void
  | VoidPointer
  | VdpauSurfaceNV
  deriving (Eq, Show)

-- | The boolean is true if it is an in type, false if it is out.
-- TODO maybe the String should be specialized.
data ParamType = ParamType String Bool Passing
  deriving (Eq, Show)

data Passing =
    Value
  | Array String Bool
  -- ^ The boolean is true if it is retained, false otherwise.
  -- TODO The String should be specialized.
  | Reference
  deriving (Eq, Show)

data Question = Mark | Number Int
  deriving (Eq, Show)

data FExtension = Soft | Winsoft | NV10 | NV20 | NV50
  deriving (Eq, Show)

data Wglflag =
    WglClientHandcode | WglServerHandcode | WglSmallData | WglBatchable
  deriving (Eq, Show)

data Dlflag = DlNotlistable | DlHandcode | DlPrepad
  deriving (Eq, Show)

data Glxflag =
    GlxClientHandcode | GlxServerHandcode | GlxClientIntercept
  | GlxEXT | GlxSGI | GlxARB | GlxIgnore
  deriving (Eq, Show)

data Glfflag =
    GlfCaptureExecute | GlfCaptureHandcode | GlfDecodeHandcode
  | GlfPixelPack | GlfPixelUnpack | GlfGlEnum | GlfIgnore
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

field :: String -> P ()
field s = () <$ (blanks1 >> token s)

question :: P Question
question =
  Number . read <$> many1 digit <* opt "re"
  -- TODO the 're' suffix is ignored, see if it is meaningful
  -- It is only used in the glxropcode of PointParameteriv, line 5108
  <|> Mark <$ string "?"

version :: P (Int,Int)
version = (,) <$> (digit' <* char '.') <*> digit'

pFunLine :: P FunLine
pFunLine = choice
  [ try (FComment <$> pComment)
  , try (FBlankLine <$ pBlankLine)
  , try pFPassthru
  , try pProperty
  , try pNewCategory
  , try pFunction
  , try pProp
  , pAt
  ]

pFPassthru :: P FunLine
pFPassthru = FPassthru <$> (string "passthru:" *> many (noneOf "\n")) <* eol

-- The hardcoded parser could directly use a single literal string instead
-- of the repeated use of 'token'.
pProperty :: P FunLine
pProperty = Property <$> choice (map try
  [ RequiredProps <$ string "required-props:"
  , ParamProp <$ (token "param:" *> token "retval" *> string "retained")
  , DlflagsProp <$ (token "dlflags:" *> token "notlistable" *>
    string "handcode" *> optional (blanks *> string "prepad"))
  , GlxflagsProp <$ (token "glxflags:" *>
    token "client-intercept" *> token "client-handcode" *>
    token "server-handcode" *> token "EXT" *> token "SGI" *>
    token "ignore" *> string "ARB")
  , VectorequivProp <$ (token "vectorequiv:" *> string "*")
  , CategoryProp <$> (token "category:" *> many (pCategory <* blanks))
  , VersionProp <$> (token "version:" *> many (version <* blanks))
  , DeprecatedProp <$> (token "deprecated:" *> many (version <* blanks))
  , ProfileProp <$> (token "profile:" *> many (pProfileVal <* blanks))
  , GlxsingleProp <$ (token "glxsingle:" *> string "*")
  , GlxropcodeProp <$ (token "glxropcode:" *> string "*")
  , GlxvendorprivProp <$ (token "glxvendorpriv:" *> string "*")
  , WglflagsProp <$ (token "wglflags:" *> token "client-handcode" *>
    token "server-handcode" *> token "small-data" *> string "batchable")
  , ExtensionProp <$ (token "extension:" *> token "future" *>
    token "not_implemented" *> token "soft" *> token "WINSOFT" *>
    token "NV10" *> token "NV20" *> string "NV50")
  , AliasProp <$ (token "alias:" *> string "*")
  , OffsetProp <$ (token "offset:" *> string "*")
  , GlfflagsProp <$ (token "glfflags:" *> string "*")
  , BeginendProp <$ (token "beginend:" *> string "*")
  , GlxvectorequivProp <$ (token "glxvectorequiv:" *> string "*")
  , SubcategoryProp <$ (token "subcategory:" *> string "*")
  , GlextmaskProp <$ (token "glextmask:" *> string "*")
  ]) <* eol

pNewCategory :: P FunLine
pNewCategory = NewCategory <$> (token "newcategory:" *> pCategory <*) eol

pFunction :: P FunLine
pFunction = Function <$>
  identifier <*> (char '(' *> sepBy identifier (token ",") <* char ')')
  <* eol

pProp :: P FunLine
pProp = Prop <$> choice
  [ try pReturn
  , try pParam
  , try pCategory'
  , try pFProfile
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

pReturn :: P Prop
pReturn = Return <$> (field "return" *> pReturnType) <* eol

pReturnType :: P ReturnType
pReturnType = choice
  [ try $ Boolean <$ string "Boolean"
  , BufferOffset <$ string "BufferOffset"
  , ErrorCode <$ string "ErrorCode"
  , try $ Float32   <$ string "Float32"
  , FramebufferStatus <$ string "FramebufferStatus"
  , GLEnum <$ string "GLenum"
  , HandleARB <$ string "handleARB"
  , Int32 <$ string "Int32"
  , List <$ string "List"
  , Path <$ string "Path"
  , try $ String <$ string "String"
  , Sync <$ string "sync"
  , try $ UInt32 <$ string "UInt32"
  , try $ UInt64 <$ string "UInt64"
  , try $ Void <$ string "void"
  , VoidPointer <$ string "VoidPointer"
  , VdpauSurfaceNV <$ string "vdpauSurfaceNV"
  ]

pParam :: P Prop
pParam = Param <$>
  (field "param" *> identifier_) <*> pParamType <* eol

pParamType :: P ParamType
pParamType = ParamType <$>
  identifier_ <*>
  pInOrOut <*>
  pPassing

pInOrOut :: P Bool
pInOrOut = choice
  [ True <$ token "in"
  , False <$ token "out"
  ]

pPassing :: P Passing
pPassing = Value <$ string "value"
  <|>
  Array <$>
  (token "array" *> char '[' *> many (noneOf "\n]") <* char ']') <*>
  (blanks *> opt "retained")
  <|>
  Reference <$ string "reference"

pCategory' :: P Prop
pCategory' = Category <$>
  (field "category" *> pCategory <* blanks) <*>
  (optional $ token "# old:" *> pCategory)
  <* eol

pFProfile :: P Prop
pFProfile = FProfile <$>
    (field "profile" *> pProfileVal) <* eol

pVersion :: P Prop
pVersion = FVersion <$>
  (field "version" *> digit' <* char '.') <*> digit' <* eol

pGlxropcode :: P Prop
pGlxropcode = Glxropcode <$> (field "glxropcode" *> question) <* eol

pOffset :: P Prop
pOffset = Offset <$> (field "offset" *> optional question) <* eol

pWglflags :: P Prop
pWglflags = Wglflags <$> (field "wglflags" *> many1 pWglflag) <* eol

pWglflag :: P Wglflag
pWglflag = choice
  [ WglClientHandcode <$ string "client-handcode"
  , try $ WglServerHandcode <$ string "server-handcode"
  , WglSmallData <$ string "small-data"
  , WglBatchable <$ string "batchable"
  ] <* blanks

pDlflags :: P Prop
pDlflags = Dlflags <$> (field "dlflags" *> pDlflag) <* eol

pDlflag :: P Dlflag
pDlflag = choice
  [ DlNotlistable <$ string "notlistable"
  , DlHandcode <$ string "handcode"
  , DlPrepad   <$ string "prepad"
  ]

pGlxflags :: P Prop
pGlxflags = Glxflags <$>
  (field "glxflags" *> many pGlxflag) <*>
  optional ( token "###" *> many pGlxflag)
  <* eol

pGlxflag :: P Glxflag
pGlxflag = choice
  [ try $ GlxClientHandcode <$ string "client-handcode"
  , GlxServerHandcode <$ string "server-handcode"
  , GlxClientIntercept <$ string "client-intercept"
  , GlxEXT <$ string "EXT"
  , GlxSGI <$ string "SGI"
  , GlxARB <$ string "ARB"
  , GlxIgnore <$ string "ignore"
  ] <* blanks

pGlxsingle :: P Prop
pGlxsingle = Glxsingle <$> (field "glxsingle" *> question) <* eol

pDeprecated :: P Prop
pDeprecated = Deprecated <$>
  (field "deprecated" *> digit' <* char '.') <*> digit' <* eol

pVectorequiv :: P Prop
pVectorequiv = Vectorequiv <$> (field "vectorequiv" *> identifier_) <* eol

pExtension :: P Prop
pExtension =  FExtension <$> (field "extension" *> many pFExtension) <* eol

pFExtension :: P FExtension
pFExtension = choice
  [ Soft <$ token "soft"
  , Winsoft <$ token "WINSOFT"
  , NV10 <$ try (token "NV10")
  , NV20 <$ try (token "NV20")
  , NV50 <$ token "NV50"
  ]

pGlxvendorpriv :: P Prop
pGlxvendorpriv =
  Glxvendorpriv <$> (field "glxvendorpriv" *> question) <* eol

pGlfflags :: P Prop
pGlfflags = Glfflags <$> (field "glfflags" *> many1 pGlfflag) <* eol

pGlfflag :: P Glfflag
pGlfflag = choice
  [ try $ GlfCaptureExecute <$ string "capture-execute"
  , GlfCaptureHandcode <$ string "capture-handcode"
  , GlfDecodeHandcode <$ string "decode-handcode"
  , try $ GlfPixelPack <$ string "pixel-pack"
  , GlfPixelUnpack <$ string "pixel-unpack"
  , GlfGlEnum <$ string "gl-enum"
  , GlfIgnore <$ string "ignore"
  ] <* blanks

pBeginend :: P Prop
pBeginend = AllowInside <$ (field "beginend" *> string "allow-inside") <* eol

pGlxvectorequiv :: P Prop
pGlxvectorequiv =
  Glxvectorequiv <$> (field "glxvectorequiv" *> identifier_) <* eol

pAlias :: P Prop
pAlias = Alias <$> (field "alias" *> identifier_) <* eol

pSubcategory :: P Prop
pSubcategory = Subcategory <$> (field "subcategory" *> identifier_) <* eol

pGlextmask :: P Prop
pGlextmask =
  Glextmask <$> (field "glextmask" *> sepBy identifier (token "|")) <* eol

----------------------------------------------------------------------
-- Predicates
----------------------------------------------------------------------

isReturn :: Prop -> Bool
isReturn (Return _) = True
isReturn _ = False

isParam :: Prop -> Bool
isParam (Param _ _) = True
isParam _ = False

isCategory :: Prop -> Bool
isCategory (Category _ _) = True
isCategory _ = False

isSubcategory :: Prop -> Bool
isSubcategory (Subcategory _) = True
isSubcategory _ = False

isProfile :: Prop -> Bool
isProfile (FProfile _) = True
isProfile _ = False

isFVersion :: Prop -> Bool
isFVersion (FVersion _ _) = True
isFVersion _ = False

isGlxropcode :: Prop -> Bool
isGlxropcode (Glxropcode _) = True
isGlxropcode _ = False

isOffset :: Prop -> Bool
isOffset (Offset _) = True
isOffset _ = False

isWglflags :: Prop -> Bool
isWglflags (Wglflags _) = True
isWglflags _ = False

isDlflags :: Prop -> Bool
isDlflags (Dlflags _) = True
isDlflags _ = False

isGlxflags :: Prop -> Bool
isGlxflags (Glxflags _ _) = True
isGlxflags _ = False

isGlxsingle :: Prop -> Bool
isGlxsingle (Glxsingle _) = True
isGlxsingle _ = False

isDeprecated :: Prop -> Bool
isDeprecated (Deprecated _ _) = True
isDeprecated _ = False

isFExtension :: Prop -> Bool
isFExtension (FExtension _) = True
isFExtension _ = False

isGlxvendorpriv :: Prop -> Bool
isGlxvendorpriv (Glxvendorpriv _) = True
isGlxvendorpriv _ = False

isGlfflags :: Prop -> Bool
isGlfflags (Glfflags _) = True
isGlfflags _ = False

isAllowInside :: Prop -> Bool
isAllowInside AllowInside = True
isAllowInside _ = False

isVectorequiv :: Prop -> Bool
isVectorequiv (Vectorequiv _) = True
isVectorequiv _ = False

isGlxvectorequiv :: Prop -> Bool
isGlxvectorequiv (Glxvectorequiv _) = True
isGlxvectorequiv _ = False

isAlias :: Prop -> Bool
isAlias (Alias _) = True
isAlias _ = False

isGlextmask :: Prop -> Bool
isGlextmask (Glextmask _) = True
isGlextmask _ = False

