{-# Language DeriveDataTypeable, NamedFieldPuns #-}
module Main where

import Paths_opengl_api (version)
import Data.Version (showVersion)
import System.Console.CmdArgs

import Control.Applicative ((<$>))

import Text.OpenGL.Spec
import Text.OpenGL.ExtHeader
import Text.OpenGL.GenChecks (mkChecks)

versionString :: String
versionString =
  "opengl-api " ++ showVersion version ++"\n\
\Copyright (c) 2010 Vo Minh Thu.\n\
\This is open source software. See the LICENSE file for conditions."

main :: IO ()
main = do
  cmd <- cmdArgs versionString [header, checks]
  processCmd cmd

data Cmd =
    Header { tmFn :: String, esFn :: String, fsFn :: String }
  | Checks { tmFn :: String, esFn :: String, fsFn :: String }
  deriving (Show, Eq, Data, Typeable)

header :: System.Console.CmdArgs.Mode Cmd
header = mode $ Header
  { tmFn = def &= typFile & text "type map" & empty "gl.tm"
  , esFn = def &= typFile & text "enumerations" & empty "enumext.spec"
  , fsFn = def &= typFile & text "functions" & empty "gl.spec"
  } &= text "Produce a C header file (normally called glext.h)."

checks :: System.Console.CmdArgs.Mode Cmd
checks = mode $ Checks
  { tmFn = def &= typFile & text "type map" & empty "gl.tm"
  , esFn = def &= typFile & text "enumerations" & empty "enumext.spec"
  , fsFn = def &= typFile & text "functions" & empty "gl.spec"
  } &= text "Produce a drop-in replacement for gl.h with glGetError checks."

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


