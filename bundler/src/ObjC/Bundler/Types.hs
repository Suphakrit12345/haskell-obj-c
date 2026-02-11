-- | Shared types for the macOS app bundler.
module ObjC.Bundler.Types
  ( BundleConfig(..)
  , DylibInfo(..)
  ) where

-- | Configuration for creating a .app bundle.
data BundleConfig = BundleConfig
  { bcExecutable :: FilePath
    -- ^ Path to the built executable binary.
  , bcName       :: String
    -- ^ Human-readable application name (e.g. "Counter").
  , bcIdentifier :: String
    -- ^ Bundle identifier (e.g. "com.haskell-objc.counter").
  , bcOutput     :: FilePath
    -- ^ Output path for the .app bundle (e.g. "dist/Counter.app").
  , bcIcon       :: Maybe FilePath
    -- ^ Optional path to an .icns icon file.
  , bcSign       :: Maybe String
    -- ^ Optional codesigning identity (e.g. "Developer ID Application: ...").
  } deriving (Show, Eq)

-- | Information about a single dynamic library dependency.
data DylibInfo = DylibInfo
  { diOrigPath   :: FilePath
    -- ^ Path as reported by @otool -L@ (may be absolute or \@rpath/...).
  , diRealPath   :: FilePath
    -- ^ Canonical (resolved) path on disk.
  , diBundleName :: String
    -- ^ Filename to use inside @Contents/Frameworks/@.
  } deriving (Show, Eq, Ord)
