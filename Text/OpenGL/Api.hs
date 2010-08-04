module Text.OpenGL.Api where

import Control.Applicative ((<$>))
import Data.List (find, groupBy, intersperse, nubBy, partition)
import Data.Char (toUpper)
import qualified Data.Map as M
import Data.Function (on)
import Numeric (showHex)

import System.IO.Unsafe (unsafePerformIO)

import qualified Text.OpenGL.Spec as Spec
import Text.OpenGL.Spec hiding (Function)

type TypeMap = M.Map String (TmType,Bool)

data Function = Function
  { funReturnType :: ReturnType
  , funName :: String
  , funParameters :: [Parameter]
  , funCategory :: Category
  , funOldCategory :: Maybe Category
  , funSubcategory :: Maybe String
  , funVersion :: Maybe (Int,Int)
  , funGlxropcode :: Maybe Question
  , funOffset :: Maybe (Maybe Question)
  , funWglflags :: Maybe [Wglflag]
  , funDlflags :: Maybe Dlflag
  , funGlxflags :: Maybe ([Glxflag],Maybe [Glxflag])
  , funGlxsingle :: Maybe Question
  , funDeprecated :: Maybe (Int,Int)
  , funExtension :: Maybe [FExtension]
  , funGlxvendorpriv :: Maybe Question
  , funGlfflags :: Maybe [Glfflag]
  , funAllowInside :: Bool
  , funVectorequiv :: Maybe String
  , funGlxvectorequiv :: Maybe String
  , funAlias :: Maybe String
  , funGlextmask :: Maybe [String]
  }
  deriving Show

data Parameter = Parameter String String Bool Passing
  deriving Show

mkFunction :: String -> [String] -> [Spec.Prop] -> Function
mkFunction a b (Spec.Return r:ps) =
  if length b /= length params
  then error $ "The list of properties hasn't the same number of " ++
         "parameters than the list af arguments."
  else Function
    { funReturnType = r
    , funName = a
    , funParameters = args
    , funCategory = c
    , funOldCategory = c'
    , funSubcategory = extractSubcategory ps''
    , funVersion = extractFVersion ps''
    , funGlxropcode = extractGlxropcode ps''
    , funOffset = extractOffset ps''
    , funWglflags = extractWglflags ps''
    , funDlflags = extractDlflags ps''
    , funGlxflags = extractGlxflags ps''
    , funGlxsingle = extractGlxsingle ps''
    , funDeprecated = extractDeprecated ps''
    , funExtension = extractFExtension ps''
    , funGlxvendorpriv = extractGlxvendorpriv ps''
    , funGlfflags = extractGlfflags ps''
    , funAllowInside = extractAllowInside ps''
    , funVectorequiv = extractVectorequiv ps''
    , funGlxvectorequiv = extractGlxvectorequiv ps''
    , funAlias = extractAlias ps''
    , funGlextmask = extractGlextmask ps''
    }
  where
  (params,ps') = partition f ps
  ([Spec.Category c c'],ps'') = partition h ps'
  f (Spec.Param _ _) = True
  f _ = False
  h (Spec.Category _ _) = True
  h _ = False
  args = zipWith g b params
  g x0 (Spec.Param x1 (Spec.ParamType x y z))
    | x0 == x1 = Parameter x0 x y z
    | otherwise = error "argument and parameter don't match"

mkFunction _ _ _ =
  error "The list of properties doesn't begin with the return type."

mkTypeMap xs = foldr f M.empty xs
  where f (Spec.TmComment _) m = m
        f (Spec.TmEntry x t b) m = M.insert x (t,b) m

extractFunctions' :: [Spec.FunLine] -> [(String,[String],[Spec.Prop])]
extractFunctions' xs = go y ys
  where
  (y:ys) = filter f xs
  f (Spec.Function _ _) = True
  f (Spec.Prop _) = True
  f _ = False
  g (Spec.Function _ _) = True
  g _ = False
  e (Spec.Prop p) = p
  go (Spec.Function a b) zs =
    let (props,r) = break g zs
    in case r of
      (fun2:rest) -> (a,b,map e props) : go fun2 rest
      [] -> [(a,b,map e props)]

extractFunctions :: [Spec.FunLine] -> [Function]
extractFunctions = map (\(a,b,c) -> mkFunction a b c) . extractFunctions'

extractSubcategory l = case filter isSubcategory l of
  [] -> Nothing
  [Spec.Subcategory s] -> Just s
  _ -> error "More than one element"

extractFVersion l = case filter isFVersion l of
  [] -> Nothing
  [Spec.FVersion i j] -> Just (i,j)
  [_,Spec.FVersion i j] -> Just (i,j) -- TODO some functions have two version properties
  _ -> error "More than one element"

extractGlxropcode l = case filter isGlxropcode l of
  [] -> Nothing
  [Spec.Glxropcode q] -> Just q
  _ -> error "More than one element"

extractOffset l = case filter isOffset l of
  [] -> Nothing
  [Spec.Offset mq] -> Just mq
  _ -> error "More than one element"

extractWglflags l = case filter isWglflags l of
  [] -> Nothing
  [Spec.Wglflags fs] -> Just fs
  _ -> error "More than one element"

extractDlflags l = case filter isDlflags l of
  [] -> Nothing
  [Spec.Dlflags fs] -> Just fs
  _ -> error "More than one element"

extractGlxflags l = case filter isGlxflags l of
  [] -> Nothing
  [Spec.Glxflags fs mfs] -> Just (fs,mfs)
  [_,Spec.Glxflags fs mfs] -> Just (fs,mfs) -- TODO some have two properties
  _ -> error "More than one element"

extractGlxsingle l = case filter isGlxsingle l of
  [] -> Nothing
  [Spec.Glxsingle q] -> Just q
  _ -> error "More than one element"

extractDeprecated l = case filter isDeprecated l of
  [] -> Nothing
  [Spec.Deprecated i j] -> Just (i,j)
  _ -> error "More than one element"

extractFExtension l = case filter isFExtension l of
  [] -> Nothing
  [Spec.FExtension es] -> Just es
  _ -> error "More than one element"

extractGlxvendorpriv l = case filter isGlxvendorpriv l of
  [] -> Nothing
  [Spec.Glxvendorpriv q] -> Just q
  _ -> error "More than one element"

extractGlfflags l = case filter isGlfflags l of
  [] -> Nothing
  [Spec.Glfflags fs] -> Just fs
  _ -> error "More than one element"

extractAllowInside l = case filter isAllowInside l of
  [] -> False
  [Spec.AllowInside] -> True
  _ -> error "More than one element"

extractVectorequiv l = case filter isVectorequiv l of
  [] -> Nothing
  [Spec.Vectorequiv s] -> Just s
  _ -> error "More than one element"

extractGlxvectorequiv l = case filter isGlxvectorequiv l of
  [] -> Nothing
  [Spec.Glxvectorequiv s] -> Just s
  _ -> error "More than one element"

extractAlias l = case filter isAlias l of
  [] -> Nothing
  [Spec.Alias s] -> Just s
  _ -> error "More than one element"

extractGlextmask l = case filter isGlextmask l of
  [] -> Nothing
  [Spec.Glextmask ss] -> Just ss
  _ -> error "More than one element"

showFunction f = unlines $
  [ show $ funReturnType f
  , funName f
  ] ++
  map show (funParameters f) ++
  [ show $ funCategory f
  , show $ funOldCategory f
  , show $ funSubcategory f
  , show $ funVersion f
  , show $ funGlxropcode f
  , show $ funOffset f
  , show $ funWglflags f
  , show $ funDlflags f
  , show $ funGlxflags f
  , show $ funGlxsingle f
  , show $ funDeprecated f
  , show $ funExtension f
  , show $ funGlxvendorpriv f
  , show $ funGlfflags f
  , show $ funAllowInside f
  , show $ funVectorequiv f
  , show $ funGlxvectorequiv f
  , show $ funAlias f
  , show $ funGlextmask f
  ]

