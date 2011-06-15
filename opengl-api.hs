{-# Language DeriveDataTypeable, NamedFieldPuns #-}
module Main where

import Paths_opengl_api (version)
import Data.Version (showVersion)
import System.Console.CmdArgs.Implicit

import Control.Applicative ((<$>))

import Text.OpenGL.Spec
import Text.OpenGL.ExtHeader
import Text.OpenGL.GenChecks (mkChecks)

versionString :: String
versionString =
  "opengl-api " ++ showVersion version ++"\n\
\Copyright (c) 2010-2011 Vo Minh Thu.\n\
\This is open source software. See the LICENSE file for conditions."

main :: IO ()
main = (>>= processCmd) . cmdArgs $
  modes
   [ header, checks
   ] 
  &= summary versionString
  &= program "opengl-api"

data Cmd =
    Header { tmFn :: String, esFn :: String, fsFn :: String }
  | Checks { tmFn :: String, esFn :: String, fsFn :: String }
  deriving (Show, Eq, Data, Typeable)

header :: Cmd
header = Header
  { tmFn = "gl.tm" &= typFile &= help "The type map file"
    &= explicit &= name "t" &= name "type-map"
  , esFn = "enumext.spec" &= typFile &= help "The enumerations file"
    &= explicit &= name "e" &= name "enumerations"
  , fsFn = "gl.spec" &= typFile &= help "The functions file"
    &= explicit &= name "f" &= name "functions"
  } &= help "Produce a C header file (normally called glext.h)."

checks :: Cmd
checks = Checks
  { tmFn = "gl.tm" &= typFile &= help "The type map file"
    &= explicit &= name "t" &= name "type-map"
  , esFn = "enumext.spec" &= typFile &= help "The enumerations file"
    &= explicit &= name "e" &= name "enumerations"
  , fsFn = "gl.spec" &= typFile &= help "The functions file"
    &= explicit &= name "f" &= name "functions"
  } &= help "Produce a drop-in replacement for gl.h with glGetError checks."

processCmd :: Cmd -> IO ()
processCmd (Header {tmFn,esFn,fsFn})= do
  tm' <- tmLines <$> readFile tmFn
  es' <- enumLines <$> readFile esFn
  fs' <- funLines <$> readFile fsFn
  case (tm',es',fs') of
    (Left err,_,_) -> putStrLn $ tmFn ++":\n" ++ show err
    (_,Left err,_) -> putStrLn $ esFn ++ ":\n" ++ show err
    (_,_,Left err) -> putStrLn $ fsFn ++ ":\n" ++ show err
    (Right tm,Right es,Right fs) -> putStrLn $ mkHeader tm es fs 63

processCmd (Checks {tmFn,esFn,fsFn})= do
  tm' <- tmLines <$> readFile tmFn
  es' <- enumLines <$> readFile esFn
  fs' <- funLines <$> readFile fsFn
  case (tm',es',fs') of
    (Left err,_,_) -> putStrLn $ tmFn ++":\n" ++ show err
    (_,Left err,_) -> putStrLn $ esFn ++ ":\n" ++ show err
    (_,_,Left err) -> putStrLn $ fsFn ++ ":\n" ++ show err
    (Right tm,Right es,Right fs) -> do
      let (h,c) = mkChecks tm es fs
      writeFile "gl-checks.h" h
      -- writeFile "gl-checks.c" c -- TODO make one with macro only, and one with macro + c code


