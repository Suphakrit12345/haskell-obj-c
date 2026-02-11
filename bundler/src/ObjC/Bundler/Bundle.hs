{-# LANGUAGE OverloadedStrings #-}

-- | Top-level .app bundle assembly.
--
-- Creates the macOS bundle directory structure, copies the executable and all
-- non-system dylibs, rewrites load paths, generates Info.plist, and optionally
-- codesigns.
module ObjC.Bundler.Bundle
  ( createBundle
  ) where

import Control.Monad (forM_, when)
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , removeDirectoryRecursive
  , setPermissions
  , getPermissions
  , setOwnerWritable
  , setOwnerExecutable
  , setOwnerReadable
  )
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeFileName)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

import ObjC.Bundler.Dylib (chaseDeps, rewritePaths)
import ObjC.Bundler.Plist (PlistValue(..), renderPlist)
import ObjC.Bundler.Types (BundleConfig(..), DylibInfo(..))

-- | Create a self-contained macOS .app bundle from a built executable.
createBundle :: BundleConfig -> IO ()
createBundle cfg = do
  let appDir      = bcOutput cfg
      contentsDir = appDir </> "Contents"
      macosDir    = contentsDir </> "MacOS"
      fwDir       = contentsDir </> "Frameworks"
      resDir      = contentsDir </> "Resources"
      execName    = takeFileName (bcExecutable cfg)
      exeDst      = macosDir </> execName

  -- Clean existing bundle if present.
  exists <- doesDirectoryExist appDir
  when exists $ do
    hPutStrLn stderr $ "Removing existing bundle: " <> appDir
    removeDirectoryRecursive appDir

  -- 1. Create directory structure.
  hPutStrLn stderr $ "Creating bundle: " <> appDir
  createDirectoryIfMissing True macosDir
  createDirectoryIfMissing True fwDir
  createDirectoryIfMissing True resDir

  -- 2. Copy executable.
  hPutStrLn stderr $ "Copying executable: " <> bcExecutable cfg
  copyFile (bcExecutable cfg) exeDst
  makeWritableExecutable exeDst

  -- 3. Chase dylib dependencies.
  hPutStrLn stderr "Chasing dynamic library dependencies..."
  dylibs <- chaseDeps (bcExecutable cfg)
  hPutStrLn stderr $ "  Found " <> show (Set.size dylibs) <> " non-system dylib(s)"

  -- 4. Copy each dylib into Frameworks/.
  forM_ (Set.toList dylibs) $ \dinfo -> do
    let dst = fwDir </> diBundleName dinfo
    hPutStrLn stderr $ "  Copying " <> diBundleName dinfo
    copyFile (diRealPath dinfo) dst
    makeWritableExecutable dst

  -- 5. Rewrite all load paths.
  hPutStrLn stderr "Rewriting load paths..."
  rewritePaths exeDst fwDir dylibs

  -- 6. Write Info.plist.
  hPutStrLn stderr "Writing Info.plist..."
  let plistBytes = renderPlist (infoPlistEntries cfg)
  LBS.writeFile (contentsDir </> "Info.plist") plistBytes

  -- 7. Write PkgInfo.
  writeFile (contentsDir </> "PkgInfo") "APPL????"

  -- 8. Copy icon if provided.
  case bcIcon cfg of
    Just iconPath -> do
      hPutStrLn stderr $ "Copying icon: " <> iconPath
      copyFile iconPath (resDir </> "AppIcon.icns")
    Nothing -> pure ()

  -- 9. Codesign if requested.
  case bcSign cfg of
    Just identity -> codesign appDir identity
    Nothing -> pure ()

  hPutStrLn stderr $ "Bundle created: " <> appDir

-- | Build the Info.plist key-value entries for a 'BundleConfig'.
infoPlistEntries :: BundleConfig -> [(Text, PlistValue)]
infoPlistEntries cfg = concat
  [ [ ("CFBundleExecutable",          PlistString (T.pack (takeFileName (bcExecutable cfg))))
    , ("CFBundleIdentifier",          PlistString (T.pack (bcIdentifier cfg)))
    , ("CFBundleName",                PlistString (T.pack (bcName cfg)))
    , ("CFBundleVersion",             PlistString "1")
    , ("CFBundleShortVersionString",  PlistString "0.1.0")
    , ("CFBundlePackageType",         PlistString "APPL")
    , ("CFBundleInfoDictionaryVersion", PlistString "6.0")
    , ("NSHighResolutionCapable",     PlistBool True)
    , ("NSPrincipalClass",            PlistString "NSApplication")
    ]
  , [ ("CFBundleIconFile", PlistString "AppIcon") | isJust (bcIcon cfg) ]
  ]

-- | Make a file writable and executable (needed after copying so
-- install_name_tool can modify it).
makeWritableExecutable :: FilePath -> IO ()
makeWritableExecutable path = do
  perms <- getPermissions path
  setPermissions path
    (setOwnerWritable True (setOwnerExecutable True (setOwnerReadable True perms)))

-- | Run @codesign --force --deep --sign@ on the app bundle.
codesign :: FilePath -> String -> IO ()
codesign appDir identity = do
  hPutStrLn stderr $ "Codesigning with identity: " <> identity
  (ex, _out, err) <- readProcessWithExitCode
    "codesign" ["--force", "--deep", "--sign", identity, appDir] ""
  case ex of
    ExitSuccess -> hPutStrLn stderr "  Codesigning succeeded."
    ExitFailure code -> do
      hPutStrLn stderr $ "  Codesigning failed (exit " <> show code <> "):"
      hPutStrLn stderr err
