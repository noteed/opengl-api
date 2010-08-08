-- Generate macros to always call glGetError after any OpenGL command and
-- report the error if any.
-- There is also the possiblity (commented for now) to generate macros and
-- c code. The advantage is that the types are specified in the signature
-- of the generated functions; the disadvantage is that the generated code
-- include functions defined in the glext header, but not necessarily
-- available in the libGL.
-- TODO the macros use a call to print_gl_error, which should be provided
-- by the user.
-- TODO there are more functions that can be called between begin/end than
-- the one with funAllowInside (glGetError can't be called betwee begin/end.
module Text.OpenGL.GenChecks where

import Data.List (intersperse)
import qualified Data.Map as M

import Text.OpenGL.Spec
import Text.OpenGL.Api
import Text.OpenGL.ExtHeader

-- TODO the data given to mkChecks should be the higher-level Api data,
-- not the line-oriented Spec data.
mkChecks :: [TmLine] -> [EnumLine] -> [FunLine] -> (String,String)
mkChecks tls els fls = (hdefines' tm fs, cdefines tm fs)
--mkChecks tls els fls = (hdefines fs, cdefines tm fs)
  where
  fs = filter f $ extractFunctions fls
  tm = mkTypeMap tls
  f x = not (funName x `elem`
    [ "GetError"
    , "MapNamedBufferRangeEXT" -- Not in my local glext.h, TODO make it an option
    , "FlushMappedNamedBufferRangeEXT"
    , "NamedCopyBufferSubDataEXT"
    , "Begin"
    , "End"
    ]) && not (funAllowInside x)

hdefines :: [Function] -> String
hdefines fs = unlines $
  [ "#ifndef GL_WITH_CHECKS_H"
  , "#define GL_WITH_CHECKS_H"
  ] ++ map hdefine fs ++
  [ "#endif /* GL_WITH_CHECKS_H */"
  ]

hdefines' :: TypeMap -> [Function] -> String
hdefines' tm fs = unlines $
  [ "#ifndef GL_WITH_CHECKS_H"
  , "#define GL_WITH_CHECKS_H"
  ] ++ map (hdefine' tm) fs ++
  [ "#endif /* GL_WITH_CHECKS_H */"
  ]

cdefines :: TypeMap -> [Function] -> String
cdefines tm fs = unlines $
  [ "#include <stdio.h>"
  , "#define GL_GLEXT_PROTOTYPES"
  , "#include <GL/gl.h>"
  ] ++ map (cdefine tm) fs

hdefine :: Function -> String
hdefine x = let n = funName x in "#define gl" ++ n ++ "(...) xx" ++
  n ++ "(__FILE__, __LINE__, __VA_ARGS__)"

hdefine' :: TypeMap -> Function -> String
hdefine' tm x = unlines $
  [ "#define gl" ++ funName x ++ "(" ++ cParameters' "" (funParameters x) ++ ") \\"
  , "gl" ++ funName x ++ "(" ++ cParameters' "" (funParameters x) ++ "); \\"
  , "{GLenum e = glGetError (); \\"
  , "if (e != GL_NO_ERROR) \\"
  , "{ \\"
  , "  fprintf (stderr, \"%s:%d: gl" ++ funName x ++ " (" ++
    cFormats tm (funParameters x) ++ "):\\n\", \\\n" ++
    "    __FILE__, __LINE__" ++ cParameters' ", " (funParameters x) ++ "); \\"
  , "  print_gl_error_ (e); \\"
  , "}}"
  ]

cdefine :: TypeMap -> Function -> String
cdefine tm x = unlines $
  [ unwords
    [ cReturnType (funReturnType x)
    , "xx" ++ funName x
    , "(char * file, int line" ++ cParameters'' tm (funParameters x) ++ ")"
    ]
  , "{"
  , (if funReturnType x /= Void
     then cReturnType (funReturnType x) ++ " r = "
     else "")
    ++ "  gl" ++ funName x ++ " (" ++ cParameters' "" (funParameters x) ++ ");"
  , "  GLenum e = glGetError ();"
  , "  if (e != GL_NO_ERROR)"
  , "  {"
  , "    fprintf (stderr, \"%s:%d: gl" ++ funName x ++ " (" ++
    cFormats tm (funParameters x) ++ "):\\n\", file, line" ++
    cParameters' ", " (funParameters x) ++ ");"
  , "  }"
  , if funReturnType x /= Void
    then "return r;\n}"
    else "}"
  ]

cParameters'' :: TypeMap -> [Parameter] -> String
cParameters'' _ [] = ""
cParameters'' tm p = ", " ++
  (concat . intersperse ", " $ map (cParameter tm) p)

cParameters' :: String -> [Parameter] -> String
cParameters' _ [] = ""
cParameters' prefix p = prefix ++
  (concat . intersperse ", " $ map cParameter' p)

cParameter' :: Parameter -> String
cParameter' (Parameter x _ _ _) = x

cFormats :: TypeMap -> [Parameter] -> String
cFormats tm ps = concat . intersperse ", " $ map (cFormat tm) ps

cFormat :: TypeMap -> Parameter -> String
cFormat tm (Parameter _ t _ p) = t'
  where
  t' = case M.lookup t tm of
    Nothing -> "Nothing"
    Just (a,b) -> case p of
      Array _ _ -> "%p"
      Reference -> "%p"
      Value -> if b then "%p" else case a of
		GLbitfield -> "%u"
		GLboolean -> "%u"
		GLbyte -> "%d"
		GLchar -> "%d"
		GLcharARB -> "%d"
		GLclampd -> "%f"
		GLclampf -> "%f"
		GLdouble -> "%f"
		GLenum -> "%u"
		GLfloat -> "%f"
		GLhalfNV -> "%d" -- TODO for now, diplayed as unsigned short
		GLhandleARB -> "%u"
		GLint -> "%d"
		GLint64 -> "%d"
		GLint64EXT -> "%d"
		GLintptr -> "%td"
		GLintptrARB -> "%td"
		GLshort -> "%d"
		GLsizei -> "%td"
		GLsizeiptr -> "%td"
		GLsizeiptrARB -> "%td"
		GLsync -> "%p"
		GLubyte -> "%u"
		ConstGLubyte -> "%u"
		GLuint -> "%u"
		GLuint64 -> "%llu"
		GLuint64EXT -> "%llu"
		GLUnurbs -> "GLUnurbs"
		GLUquadric -> "GLUquadric"
		GLushort -> "%u"
		GLUtesselator -> "GLUtesselator"
		GLvoid -> "%p"
		Star -> "%p"
		UnderscoreGLfuncptr -> "%p"
		GLvoidStarConst -> "%p"

