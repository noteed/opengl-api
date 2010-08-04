-- A few checks on the raw function data[0]. The Function representation
-- of this module is written w.r.t. these checks.
--
-- [0]: That is, (String,[String],[Prop]).

-- For instance, to check if every function has a category:
-- all (has isCategory) <$> extractFunctions'_ "spec-files/opengl/gl.spec"

module Text.OpenGL.Checks where

import Text.OpenGL.Spec

has :: (Prop -> Bool) -> (a,b,[Prop]) -> Bool
has f (_,_,x) = any f x

returns = all (has isReturn)

params = all (has isParam)

categories = all (has isCategory)

subcategories = all (has isSubcategory)

versions = all (has isFVersion)

glxropcodes = all (has isGlxropcode)

offsets = all (has isOffset)

wglflags = all (has isWglflags)

dlflags = all (has isDlflags)

glxflags = all (has isGlxflags)

glxsingles = all (has isGlxsingle)

deprecateds = all (has isDeprecated)

extensions = all (has isFExtension)

glxvendorprivs = all (has isGlxvendorpriv)

glfflags = all (has isGlfflags)

allowinsides = all (has isAllowInside)

vectorequivs = all (has isVectorequiv)

glxvectorequivs = all (has isGlxvectorequiv)

alias = all (has isAlias)

glextmasks = all (has isGlextmask)

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

