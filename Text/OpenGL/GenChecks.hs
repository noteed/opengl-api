module Text.OpenGL.GenChecks where

import Data.List (intersperse)

import Text.OpenGL.Spec
import Text.OpenGL.Api
import Text.OpenGL.ExtHeader

{-
void
hdEnableVertexAttribArray (char * file, int line, GLuint index)
{ 
  GLenum e = glGetError ();
  if (e != GL_NO_ERROR)
  { 
    fprintf (stderr, "%s:%d: Remaining OpenGL error:\n", file, line);
    print_gl_error_ (e);
  }
  glEnableVertexAttribArray (index);
  e = glGetError ();
  if (e != GL_NO_ERROR)
  { 
    fprintf (stderr, "%s:%d: glEnableVertexAttribArray (%d):\n", file, line, index);
    print_gl_error_ (e);
  }
}

#define glEnableVertexAttribArray hdEnableVertexAttribArray
-}

mkChecks :: [TmLine] -> [EnumLine] -> [FunLine] -> (String,String)
mkChecks tls els fls =
  (unlines $ map hdefine fs,
   unlines $ map (cdefine $ mkTypeMap tls) fs)
  where
  fs = extractFunctions fls

hdefine :: Function -> String
hdefine x = let n = funName x in "#define gl" ++ n ++ " _" ++ n

cdefine :: TypeMap -> Function -> String
cdefine tm x = unlines $
  [ unwords
    [ cReturnType (funReturnType x)
    , "_" ++ funName x
    , "(" ++ cParameters tm (funParameters x) ++ ")"
    ]
  , "{"
  , "  gl" ++ funName x ++ " (" ++ cParameters' (funParameters x) ++ ");"
  , "}"
  ]

cParameters' :: [Parameter] -> String
cParameters' [] = ""
cParameters' p = concat . intersperse ", " $ map cParameter' p

cParameter' :: Parameter -> String
cParameter' (Parameter x _ _ _) = x
