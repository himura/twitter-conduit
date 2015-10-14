#!/usr/bin/env runhaskell

-- This code is mostly borrowed from
-- https://github.com/ekmett/lens/blob/4b032a0047f9ecf687947541211487c9d6244f3e/Setup.lhs

import Data.List (nub)
import Data.Version (showVersion)
import Distribution.Package (PackageName(PackageName), PackageId, InstalledPackageId, packageVersion, packageName)
import Distribution.PackageDescription (PackageDescription(), TestSuite(..))
import Distribution.Simple (defaultMainWithHooks, UserHooks(..), simpleUserHooks)
import Distribution.Simple.Utils (rewriteFile, createDirectoryIfMissingVerbose)
import Distribution.Simple.BuildPaths (autogenModulesDir)
import Distribution.Simple.Setup (BuildFlags(buildVerbosity), fromFlag)
import Distribution.Simple.LocalBuildInfo (withLibLBI, withTestLBI, LocalBuildInfo(), ComponentLocalBuildInfo(componentPackageDeps))
import Distribution.Verbosity (Verbosity)
import System.FilePath ((</>))

main :: IO ()
main = defaultMainWithHooks hooks

hooks :: UserHooks
hooks =
    simpleUserHooks
    { buildHook = \pkg lbi hooks flags -> do
           generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
           buildHook simpleUserHooks pkg lbi hooks flags
    }

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkg lbi = do
  let dir = autogenModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  withLibLBI pkg lbi $ \_ libcfg ->
    withTestLBI pkg lbi $ \suite suitecfg ->
      rewriteFile (dir </> "Build_" ++ testName suite ++ ".hs") $ unlines
        [ "module Build_" ++ testName suite ++ " where"
        , "deps :: [String]"
        , "deps = " ++ show (formatdeps (testDeps libcfg suitecfg))
        ]
  where
    formatdeps = map (formatone . snd)
    formatone p = case packageName p of
      PackageName n -> n ++ "-" ++ showVersion (packageVersion p)

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(InstalledPackageId, PackageId)]
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys
