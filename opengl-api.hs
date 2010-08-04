{-# Language DeriveDataTypeable, NamedFieldPuns #-}
module Main where

import Paths_opengl_api (version)
import Data.Version (showVersion)
import System.Console.CmdArgs

import Control.Applicative ((<$>))

import Text.OpenGL.Spec
import Text.OpenGL.ExtHeader

versionString :: String
versionString =
  "opengl-api " ++ showVersion version ++"\n\
\Copyright (c) 2010 Vo Minh Thu.\n\
\This is open source software. See the LICENSE file for conditions."

main :: IO ()
main = do
  cmd <- cmdArgs versionString [header]
  processCmd cmd

data Cmd =
    Header { tmFn :: String, esFn :: String, fsFn :: String }
  deriving (Show, Eq, Data, Typeable)

header :: System.Console.CmdArgs.Mode Cmd
header = mode $ Header
  { tmFn = def &= typFile & text "type map" & empty "gl.tm"
  , esFn = def &= typFile & text "enumerations" & empty "enumext.spec"
  , fsFn = def &= typFile & text "functions" & empty "gl.spec"
  } &= text "Produce a C header file (normally called glext.h)."

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

