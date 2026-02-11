{-# LANGUAGE OverloadedStrings #-}

-- | Dynamic library dependency chasing and install_name_tool rewriting.
--
-- Uses @otool@ to discover non-system dylib dependencies (recursively) and
-- @install_name_tool@ to rewrite all load paths so the resulting .app bundle
-- is fully self-contained with only relative references.
module ObjC.Bundler.Dylib
  ( chaseDeps
  , rewritePaths
  , isSystemLib
  ) where

import Control.Monad (forM_, unless, when)
import Data.Char (isSpace)
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.List (isPrefixOf, stripPrefix)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Directory (canonicalizePath, doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath (takeFileName, (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcess, readProcessWithExitCode)

import ObjC.Bundler.Types (DylibInfo(..))

-- ---------------------------------------------------------------------------
-- otool parsing
-- ---------------------------------------------------------------------------

-- | Run @otool -L@ on a Mach-O binary and return the list of referenced
-- library paths (the first line, which is the binary name, is skipped).
--
-- Each line looks like:
--
-- > \t@rpath/libHSbase-...-ghc9.10.3.dylib (compatibility version 0.0.0, current version 0.0.0)
--
-- We strip the leading tab, then take everything before the first @" ("@.
getDeps :: FilePath -> IO [String]
getDeps path = do
  out <- readProcess "otool" ["-L", path] ""
  let lns = drop 1 (lines out)  -- skip first line (binary name)
  pure (map parseDep lns)
  where
    parseDep ln =
      let trimmed = dropWhile isSpace ln
      in takeWhile (/= '(') trimmed |> reverse |> dropWhile isSpace |> reverse
    (|>) = flip ($)

-- | Run @otool -l@ on a Mach-O binary and extract all LC_RPATH path values.
--
-- The relevant portion of the output looks like:
--
-- > Load command N
-- >           cmd LC_RPATH
-- >       cmdsize 112
-- >          path /some/absolute/path (offset 12)
getRpaths :: FilePath -> IO [FilePath]
getRpaths path = do
  out <- readProcess "otool" ["-l", path] ""
  pure (extractRpaths (lines out))
  where
    extractRpaths [] = []
    extractRpaths (x:xs)
      | "cmd LC_RPATH" `isInfixOfTrimmed` x =
          case findPathLine xs of
            Just p  -> p : extractRpaths xs
            Nothing -> extractRpaths xs
      | otherwise = extractRpaths xs

    findPathLine [] = Nothing
    findPathLine (l:ls)
      | Just rest <- stripPrefix "path " (dropWhile isSpace l) =
          Just (takeWhile (/= '(') rest |> reverse |> dropWhile isSpace |> reverse)
      | "cmd " `isPrefixOf` dropWhile isSpace l = Nothing  -- next load command
      | otherwise = findPathLine ls

    isInfixOfTrimmed needle haystack =
      needle `isPrefixOf` dropWhile isSpace haystack

    (|>) = flip ($)

-- | Resolve an @\@rpath/...@ reference against a list of rpath directories.
-- Returns the first path that exists on disk, or 'Nothing'.
resolveRpath :: [FilePath] -> String -> IO (Maybe FilePath)
resolveRpath rpaths ref = case stripPrefix "@rpath/" ref of
  Nothing -> pure Nothing
  Just relName -> go rpaths relName
  where
    go [] _ = pure Nothing
    go (rp:rps) rel = do
      let candidate = rp </> rel
      exists <- doesFileExist candidate
      if exists then pure (Just candidate) else go rps rel

-- | Returns 'True' for libraries that are part of the macOS system and
-- should never be bundled.
isSystemLib :: FilePath -> Bool
isSystemLib p =
     "/usr/lib/"      `isPrefixOf` p
  || "/System/"       `isPrefixOf` p
  || "/Library/Apple/" `isPrefixOf` p

-- ---------------------------------------------------------------------------
-- Recursive dependency chasing
-- ---------------------------------------------------------------------------

-- | Recursively discover all non-system dylib dependencies of a Mach-O
-- binary, using a BFS walk. Returns a set of 'DylibInfo' records
-- deduplicated by canonical (resolved) path.
chaseDeps :: FilePath -> IO (Set.Set DylibInfo)
chaseDeps binaryPath = do
  -- We need the binary's rpaths to resolve @rpath/ references.
  rpaths <- getRpaths binaryPath

  -- BFS state: queue of (origPath, resolvedPath) to visit, visited set by realpath.
  visitedRef <- newIORef (Set.empty :: Set.Set FilePath)
  resultRef  <- newIORef (Map.empty :: Map.Map FilePath DylibInfo)

  let enqueue orig resolved = do
        real <- canonicalizePath resolved
        visited <- readIORef visitedRef
        unless (Set.member real visited) $ do
          modifyIORef' visitedRef (Set.insert real)
          let info = DylibInfo
                { diOrigPath   = orig
                , diRealPath   = real
                , diBundleName = takeFileName real
                }
          modifyIORef' resultRef (Map.insert real info)
          -- Recurse: find this dylib's own deps and rpaths.
          depRefs <- getDeps resolved
          depRpaths <- getRpaths resolved
          let allRpaths = rpaths <> depRpaths
          forM_ depRefs $ \ref -> do
            mResolved <- resolveRef allRpaths ref
            case mResolved of
              Just absPath
                | not (isSystemLib absPath) -> enqueue ref absPath
              _ -> pure ()

  -- Start with the binary's own deps.
  deps <- getDeps binaryPath
  forM_ deps $ \ref -> do
    mResolved <- resolveRef rpaths ref
    case mResolved of
      Just absPath
        | not (isSystemLib absPath) -> enqueue ref absPath
      _ -> pure ()

  result <- readIORef resultRef
  pure (Set.fromList (Map.elems result))

-- | Resolve a library reference to an absolute path on disk.
-- Handles @\@rpath/...@ references, @\@executable_path/...@, @\@loader_path/...@,
-- and already-absolute paths.
resolveRef :: [FilePath] -> String -> IO (Maybe FilePath)
resolveRef rpaths ref
  | "@rpath/" `isPrefixOf` ref = resolveRpath rpaths ref
  | "@" `isPrefixOf` ref = pure Nothing  -- @executable_path, @loader_path, etc. -- skip
  | "/" `isPrefixOf` ref = do
      exists <- doesFileExist ref
      pure (if exists then Just ref else Nothing)
  | otherwise = pure Nothing

-- ---------------------------------------------------------------------------
-- Path rewriting
-- ---------------------------------------------------------------------------

-- | Rewrite all load paths in a bundle so every reference is relative.
--
-- @rewritePaths exePath frameworksDir dylibs@ performs three passes:
--
--   1. Rewrite the executable's references and add an rpath.
--   2. Rewrite each dylib's identity, inter-dylib references, and add an rpath.
--   3. Remove stale absolute rpaths from all binaries.
rewritePaths :: FilePath -> FilePath -> Set.Set DylibInfo -> IO ()
rewritePaths exePath frameworksDir dylibs = do
  let dylibList = Set.toList dylibs

  -- Build a lookup from original-path -> bundle filename, plus realpath -> bundle filename.
  -- We need both because a binary may reference a lib by its @rpath/... name
  -- or by its absolute path.
  let origMap = Map.fromList
        [(diOrigPath d, diBundleName d) | d <- dylibList]
      realMap = Map.fromList
        [(diRealPath d, diBundleName d) | d <- dylibList]

  -- Pass 1: Executable
  hPutStrLn stderr "  Rewriting executable..."
  addRpath exePath "@executable_path/../Frameworks"
  exeDeps <- getDeps exePath
  forM_ exeDeps $ \dep ->
    case lookupBundleName origMap realMap dep of
      Just bundleName ->
        changeDep exePath dep ("@rpath/" <> bundleName)
      Nothing -> pure ()

  -- Pass 2: Each dylib in Frameworks/
  forM_ dylibList $ \dinfo -> do
    let dylibPath = frameworksDir </> diBundleName dinfo
        newId = "@rpath/" <> diBundleName dinfo
    hPutStrLn stderr $ "  Rewriting " <> diBundleName dinfo <> "..."
    setId dylibPath newId
    addRpath dylibPath "@loader_path/."
    dylibDeps <- getDeps dylibPath
    forM_ dylibDeps $ \dep ->
      case lookupBundleName origMap realMap dep of
        Just bundleName ->
          changeDep dylibPath dep ("@rpath/" <> bundleName)
        Nothing -> pure ()

  -- Pass 3: Remove stale absolute rpaths
  hPutStrLn stderr "  Removing stale rpaths..."
  let allBinaries = exePath : [frameworksDir </> diBundleName d | d <- dylibList]
  forM_ allBinaries $ \binPath -> do
    rps <- getRpaths binPath
    forM_ rps $ \rp ->
      when (isAbsoluteStaleRpath rp) $
        deleteRpath binPath rp

-- | Look up the bundle filename for a dependency path. Tries the original
-- path first (which may be an @\@rpath/...@ reference), then falls back to
-- resolving via realpath keys.
lookupBundleName
  :: Map.Map FilePath String
  -> Map.Map FilePath String
  -> FilePath
  -> Maybe String
lookupBundleName origMap realMap dep =
  case Map.lookup dep origMap of
    Just name -> Just name
    Nothing -> case stripPrefix "@rpath/" dep of
      -- If the dep is @rpath/libFoo.dylib, try to find any entry whose
      -- bundle name matches the basename.
      Just relName ->
        let baseName = takeFileName relName
        in if baseName `elem` Map.elems origMap
           then Just baseName
           else Map.lookup dep realMap
      Nothing -> Map.lookup dep realMap

-- | Returns 'True' for rpaths that are absolute filesystem paths (i.e. stale
-- references to the build machine).
isAbsoluteStaleRpath :: FilePath -> Bool
isAbsoluteStaleRpath rp = "/" `isPrefixOf` rp

-- ---------------------------------------------------------------------------
-- install_name_tool wrappers
-- ---------------------------------------------------------------------------

-- | @install_name_tool -change old new binary@
changeDep :: FilePath -> String -> String -> IO ()
changeDep binary old new =
  runInstallNameTool ["-change", old, new, binary]

-- | @install_name_tool -id newId binary@
setId :: FilePath -> String -> IO ()
setId binary newId =
  runInstallNameTool ["-id", newId, binary]

-- | @install_name_tool -add_rpath rpath binary@
addRpath :: FilePath -> String -> IO ()
addRpath binary rpath =
  runInstallNameTool ["-add_rpath", rpath, binary]

-- | @install_name_tool -delete_rpath rpath binary@
deleteRpath :: FilePath -> String -> IO ()
deleteRpath binary rpath =
  -- delete_rpath can fail if the rpath doesn't exist (e.g. already deleted),
  -- so we tolerate failures.
  runInstallNameToolTolerant ["-delete_rpath", rpath, binary]

runInstallNameTool :: [String] -> IO ()
runInstallNameTool args = do
  (ex, _out, err) <- readProcessWithExitCode "install_name_tool" args ""
  case ex of
    ExitSuccess -> pure ()
    ExitFailure code -> do
      hPutStrLn stderr $ "install_name_tool failed (exit " <> show code <> "): "
        <> unwords ("install_name_tool" : args)
      hPutStrLn stderr err
      -- If it's a header padding issue, give a helpful hint.
      when ("header" `isPrefixOf` err || "Mach-O" `isPrefixOf` err) $
        hPutStrLn stderr
          "Hint: try linking with -optl -headerpad_max_install_names"

runInstallNameToolTolerant :: [String] -> IO ()
runInstallNameToolTolerant args = do
  (_, _, _) <- readProcessWithExitCode "install_name_tool" args ""
  pure ()
