-- A few checks on the raw function data[0]. The Function representation
-- of this module is written w.r.t. these checks.
--
-- [0]: That is, (String,[String],[Prop]).

-- For instance, to check if every function has a category:
-- all (has isCategory) <$> extractFunctions'_ "spec-files/opengl/gl.spec"

module Text.OpenGL.Checks where

import Text.OpenGL.Spec

type F = [(String,[String],[Prop])]

has :: (Prop -> Bool) -> (a,b,[Prop]) -> Bool
has f (_,_,x) = any f x

returns :: F -> Bool
returns = all (has isReturn)

params :: F -> Bool
params = all (has isParam)

categories :: F -> Bool
categories = all (has isCategory)

subcategories :: F -> Bool
subcategories = all (has isSubcategory)

versions :: F -> Bool
versions = all (has isFVersion)

glxropcodes :: F -> Bool
glxropcodes = all (has isGlxropcode)

offsets :: F -> Bool
offsets = all (has isOffset)

wglflags :: F -> Bool
wglflags = all (has isWglflags)

dlflags :: F -> Bool
dlflags = all (has isDlflags)

glxflags :: F -> Bool
glxflags = all (has isGlxflags)

glxsingles :: F -> Bool
glxsingles = all (has isGlxsingle)

deprecateds :: F -> Bool
deprecateds = all (has isDeprecated)

extensions :: F -> Bool
extensions = all (has isFExtension)

glxvendorprivs :: F -> Bool
glxvendorprivs = all (has isGlxvendorpriv)

glfflags :: F -> Bool
glfflags = all (has isGlfflags)

allowinsides :: F -> Bool
allowinsides = all (has isAllowInside)

vectorequivs :: F -> Bool
vectorequivs = all (has isVectorequiv)

glxvectorequivs :: F -> Bool
glxvectorequivs = all (has isGlxvectorequiv)

alias :: F -> Bool
alias = all (has isAlias)

glextmasks :: F -> Bool
glextmasks = all (has isGlextmask)

checkPresence :: F -> IO ()
checkPresence fs = do
  let f x = if x fs
        then putStrLn "always."
        else putStrLn "not always."
  mapM_ f
    [ returns -- always
    , params
    , categories -- always
    , subcategories
    , versions
    , glxropcodes
    , offsets
    , wglflags
    , dlflags
    , glxflags
    , glxsingles
    , deprecateds
    , extensions
    , glxvendorprivs
    , glfflags
    , allowinsides
    , vectorequivs
    , glxvectorequivs
    , alias
    , glextmasks
    ]

