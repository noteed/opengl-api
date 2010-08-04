-- |
-- This module uses Text.OpenGL.Spec to create a perfect replica of the
-- glext.h file given by opengl.org from the spec files.
-- To do this, it defines its own intermediate data structures (instead
-- of using the one provided by Text.OpenGL.Api). This is necessary as
-- the passthru lines get output to the glext.h file (the representation
-- used in Text.OpenGL.Api drops those lines).
module Text.OpenGL.ExtHeader where

import Data.List (intersperse, nubBy)
import Data.Char (toUpper)
import qualified Data.Map as M
import Numeric (showHex)

import qualified Text.OpenGL.Spec as Spec
import Text.OpenGL.Spec
import Text.OpenGL.Api

-- Each category is paired with a list of defined key-value pairs.
type Enumerants = [(Category, [Enumerant])]

data Enumerant =
    Define String Value
  | EPassthru String
  | EUse String String
  deriving Show

-- Maybe the Category and [Function] can be grouped together.
data HeaderItem =
    HPassthru String
  | HNewCategory Category [String] -- The strings are passthru lines
  | HFunctions [Function] [String] -- idem
  deriving Show

cHeaderItem :: TypeMap -> HeaderItem -> [String]
cHeaderItem tm i = case i of
  HPassthru [] -> [""]
  HPassthru (_:str) -> [str]
  HNewCategory c ss ->
    [ "#ifndef GL_" ++ cCategory c
    , "#define GL_" ++ cCategory c ++ " 1"
    ] ++ map (drop 1) ss ++
    [ "#endif",""
    ]
  HFunctions fs ss ->
    [ "#ifndef GL_" ++ cCategory (funCategory $ head fs)
    , "#define GL_" ++ cCategory (funCategory $ head fs) ++ " 1"
    ] ++ map (drop 1) ss ++
    [ "#ifdef GL_GLEXT_PROTOTYPES"
    ] ++ map (cDeclaration tm) fs ++
    [ "#endif /* GL_GLEXT_PROTOTYPES */"
    ] ++ map (cDeclaration' tm) fs ++
    [ "#endif", ""
    ]

cCategory :: Category -> String
cCategory c = case c of
  Spec.Version i j b -> "VERSION_" ++ show i ++ "_" ++ show j ++
    if b then "_DEPRECATED" else ""
  Spec.Extension e s b -> Spec.showExtension e ++ "_" ++ s ++
    if b then "_DEPRECATED" else ""
  Spec.Name s -> s

extractHeaderItems :: [Spec.FunLine] -> [HeaderItem]
extractHeaderItems [] = []
extractHeaderItems xs = go xs
  where
  g (Spec.Prop _) = False
  g _ = True
  e (Spec.Prop p) = p
  go (Spec.FPassthru str : zs) =
    HPassthru str : go zs
  go (Spec.NewCategory c : zs) =
    goC (HNewCategory c []) zs
  go (Spec.Function a b : zs) =
    let (props,r) = break g zs
        f = mkFunction a b $ map e props
    in if hasHeaderCategory f
       then goF (HFunctions [f] []) r
       else go r
  go (_:zs) = go zs
  go [] = []
  goC (HNewCategory c ps) (Spec.FPassthru str : zs) =
    goC (HNewCategory c (ps++[str])) zs
  goC (HNewCategory c ps) (Spec.NewCategory c' : zs) =
    HNewCategory c ps : goC (HNewCategory c' []) zs
  goC (HNewCategory c ps) (Spec.Function a b : zs) =
    let (props,r) = break g zs
        f = mkFunction a b $ map e props
    in if hasHeaderCategory f
          then HNewCategory c ps : goF (HFunctions [f] []) r
          else goC (HNewCategory c ps) r
  goC c (_:zs) = goC c zs
  goC c [] = [c]
  goF (HFunctions fs ps) (Spec.FPassthru str : zs) =
    goF (HFunctions fs (ps++[str])) zs
  goF fs (Spec.NewCategory c' : zs) =
    fs : goC (HNewCategory c' []) zs
  goF (HFunctions fs ps) (Spec.Function a b : zs) =
    let (props,r) = break g zs
        f = mkFunction a b $ map e props
        newf = case fs of
          [] -> False
          (f':_) -> funCategory f /= funCategory f'
    in if newf
       then HFunctions fs ps : goF (HFunctions [f] []) r
       else goF (HFunctions (fs++[f]) ps) r
  goF fs (_:zs) = goF fs zs
  goF fs [] = [fs]

hasHeaderCategory :: Function -> Bool
hasHeaderCategory f = case funCategory f of
  Spec.Version 1 0 _ -> False
  Spec.Version 1 1 _ -> False
  _ -> True

-- TODO don't hardcode it, make it an option.
hasHeaderCategory' :: (Category,[Enumerant]) -> Bool
hasHeaderCategory' (c, _) = case c of
  Spec.Version 1 0 _ -> False
  Spec.Version 1 1 _ -> False
  _ -> True

cEnumeration :: (Category,[Enumerant]) -> [String]
cEnumeration e = case e of
  (c, es) ->
    [ "#ifndef GL_" ++ cCategory c
    ] ++ map cE es ++
    [ "#endif"
    , ""
    ]

cE :: Enumerant -> String
cE e = case e of
  Define a b -> "#define GL_" ++ a ++ pad a ++ " " ++ showValue b
  EPassthru str -> "/* " ++ str ++ "*/"
  EUse _ b -> "/* reuse GL_" ++ b ++ " */"

pad :: String -> String
pad s = replicate (30 - length s) ' '

-- TODO, reuse the same function from Spec.hs.
showValue :: Value -> String
showValue v = case v of
  Spec.Hex i l Nothing -> "0x" ++ showHex' l i
  Spec.Hex i l (Just Spec.U) -> "0x" ++ showHex' l i ++ "u"
  Spec.Hex i l (Just Spec.Ull) -> "0x" ++ showHex' l i ++ "ull"
  Spec.Deci i -> show i
  Spec.Identifier x -> x

showHex' :: Integral a => Int -> a -> String
showHex' l i = replicate (l - length h) '0' ++ h
  where h = map toUpper (showHex i "")

groupEnums :: [EnumLine] -> Enumerants
groupEnums xs = go xs
  where
  go (Spec.Comment _ : zs) = go zs
  go (Spec.BlankLine : zs) = go zs
  go (Spec.Start se _ : zs) = goS (se, []) zs
  go (Spec.Passthru str : zs) = error "encountering a Passthru before a Start"
  go (Spec.Enum _ _ _ : _) = error "encoutering an Enum before a Start"
  go (Spec.Use _ _ : _) = error "encoutering a Use before a Start"
  go [] = []
  goS e (Spec.Comment _ : zs) = goS e zs
  goS e (Spec.BlankLine : zs) = goS e zs
  goS e (Spec.Start se _ : zs) = e : goS (se, []) zs
  goS (se, es) (Spec.Passthru str : zs) =
    goS (se, (es++[EPassthru str])) zs
  goS (se, es) (Spec.Enum a b _ : zs) =
    goS (se, (es++[Define a b])) zs
  goS (se, es) (Spec.Use a b : zs) =
    goS (se, (es++[EUse a b])) zs
  goS e [] = [e]

-- TODO make it a special option for sanity-check.
glextHeader :: TypeMap -> [EnumLine] -> [FunLine] -> String
glextHeader typeMap enums funs = unlines $
  glextBeginning glextVersion ++
  concatMap cEnumeration es ++
  [ ""
  , "/*************************************************************/"
  , ""
  ] ++
  concatMap (cHeaderItem typeMap) hi ++
  glextEnding
  where
  glextVersion = 63
  hi = rewrite $ nubBy f $ extractHeaderItems funs
  es = map g . filter hasHeaderCategory' $ groupEnums enums
  -- The "newcategory: NV_fragment_program" is unecessary as some functions
  -- have that category.
  rewrite (HNewCategory
    (Spec.Extension Spec.NV "fragment_program" False) [p] : 
    HFunctions fs ps :
    ys) = HFunctions fs (p:ps) : rewrite ys
  rewrite (x:xs) = x : rewrite xs
  rewrite [] = []
  -- The "newcategory: MESA_ycbcr_texture" is present twice.
  f (HNewCategory (Spec.Extension Spec.MESA "ycbcr_texture" False) [])
    (HNewCategory (Spec.Extension Spec.MESA "ycbcr_texture" False ) []) = True
  f a b = False
  -- The "SGIX_ycrcb_subsample" has no enum in the original glext.h.
  -- The enum "2X_BIT_ATI" is present twice.
  g (v@(Spec.Extension Spec.SGIX "ycrcb_subsample" False), _) = (v, [])
  g (c, es) = (c, nubBy g' es)
  g' (Define "2X_BIT_ATI" _) (Define "2X_BIT_ATI" _) = True
  g' _ _ = False

glextBeginning :: Int -> [String]
glextBeginning n =
  [ "#ifndef __glext_h_"
  , "#define __glext_h_"
  , ""
  , "#ifdef __cplusplus"
  , "extern \"C\" {"
  , "#endif"
  , ""
  , "/*"
  , "** Copyright (c) 2007-2010 The Khronos Group Inc."
  , "** "
  , "** Permission is hereby granted, free of charge, to any person obtaining a"
  , "** copy of this software and/or associated documentation files (the"
  , "** \"Materials\"), to deal in the Materials without restriction, including"
  , "** without limitation the rights to use, copy, modify, merge, publish,"
  , "** distribute, sublicense, and/or sell copies of the Materials, and to"
  , "** permit persons to whom the Materials are furnished to do so, subject to"
  , "** the following conditions:"
  , "** "
  , "** The above copyright notice and this permission notice shall be included"
  , "** in all copies or substantial portions of the Materials."
  , "** "
  , "** THE MATERIALS ARE PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND,"
  , "** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF"
  , "** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT."
  , "** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY"
  , "** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,"
  , "** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE"
  , "** MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS."
  , "*/"
  , ""
  , "/* Header file version number, required by OpenGL ABI for Linux */"
  , "/* This glext.h file was auto-generated from the spec files. */"
  , "/* Current version at http://www.opengl.org/registry/ */"
  , "#define GL_GLEXT_VERSION " ++ show n
  , "/* Function declaration macros - to move into glplatform.h */"
  , ""
  , "#if defined(_WIN32) && !defined(APIENTRY) && !defined(__CYGWIN__) && !defined(__SCITECH_SNAP__)"
  , "#define WIN32_LEAN_AND_MEAN 1"
  , "#include <windows.h>"
  , "#endif"
  , ""
  , "#ifndef APIENTRY"
  , "#define APIENTRY"
  , "#endif"
  , "#ifndef APIENTRYP"
  , "#define APIENTRYP APIENTRY *"
  , "#endif"
  , "#ifndef GLAPI"
  , "#define GLAPI extern"
  , "#endif"
  , ""
  , "/*************************************************************/"
  , ""
  ]

glextEnding :: [String]
glextEnding =
  [ ""
  , "#ifdef __cplusplus"
  , "}"
  , "#endif"
  , ""
  , "#endif"
  ]

mkHeader :: [TmLine] -> [EnumLine] -> [FunLine] -> Int -> String
mkHeader tm es fs v = unlines $
  glextBeginning v ++
  concatMap cEnumeration (filter hasHeaderCategory' $ groupEnums es) ++
  [ ""
  , "/*************************************************************/"
  , ""
  ] ++
  concatMap (cHeaderItem $ mkTypeMap tm) (extractHeaderItems fs) ++
  glextEnding

cDeclaration :: TypeMap -> Function -> String
cDeclaration tm f = unwords
  [ "GLAPI"
  , cReturnType (funReturnType f)
  , "APIENTRY"
  , "gl" ++ funName f
  , "(" ++ cParameters tm (funParameters f) ++ ");"
  ]

cDeclaration' :: TypeMap -> Function -> String
cDeclaration' tm f = unwords
  [ "typedef"
  , cReturnType (funReturnType f)
  , "(APIENTRYP PFNGL" ++ map toUpper (funName f) ++ "PROC)"
  , "(" ++ cParameters tm (funParameters f) ++ ");"
  ]

-- TODO use the TypeMap, which means maybe regroug ReturnType with the
-- String to lookup the typemap.
cReturnType :: ReturnType -> String
cReturnType t = case t of
  Spec.Void -> "void"
  Spec.Boolean -> "GLboolean"
  Spec.VoidPointer -> "GLvoid*"
  Spec.UInt32 -> "GLuint"
  Spec.Int32 -> "GLint"
  Spec.String -> "const GLubyte *"
  Spec.GLEnum -> "GLenum"
  Spec.HandleARB -> "GLhandleARB"
  Spec.BufferOffset -> "GLintptr"
  Spec.FramebufferStatus -> "GLenum"
  Spec.Sync -> "GLsync"
  x -> show x

cParameters :: TypeMap -> [Parameter] -> String
cParameters tm [] = "void"
cParameters tm p = concat . intersperse ", " $ map (cParameter tm) p

cParameter :: TypeMap -> Parameter -> String
cParameter tm (Parameter x t i p) = c ++ t' ++ x
  where
  t' = case M.lookup t tm of
    Nothing -> "Nothing"
    Just (a,b) -> case p of
      Value -> cType a ++ (if b then " *" else " ")
      Array _ _ -> cType a ++ (if b then "* *" else " *")
      Reference -> cType a ++ (if b then "* *" else " *")
  c = case (p,i) of
    (Array _ _,True) -> "const "
    (Reference,True) -> "const "
    _ -> ""

cType :: TmType -> String
cType t = case t of
  Star -> "void"
  UnderscoreGLfuncptr -> "_GLfuncptr"
  GLvoidStarConst -> "GLvoid* const"
  x -> show x

