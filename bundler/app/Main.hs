-- | CLI entry point for the macOS app bundler.
--
-- Usage:
--
-- @
-- objc-bundler --executable path/to/binary --name "Counter" --identifier com.haskell-objc.counter --output dist/Counter.app
-- objc-bundler -e path/to/binary -n "Counter" -i com.haskell-objc.counter -o dist/Counter.app --icon icon.icns --sign "Developer ID Application: ..."
-- @
module Main (main) where

import Options.Applicative

import ObjC.Bundler.Bundle (createBundle)
import ObjC.Bundler.Types (BundleConfig(..))

-- ---------------------------------------------------------------------------
-- CLI parser
-- ---------------------------------------------------------------------------

bundleConfigParser :: Parser BundleConfig
bundleConfigParser = BundleConfig
  <$> strOption
        ( long "executable"
       <> short 'e'
       <> metavar "PATH"
       <> help "Path to the built executable binary"
        )
  <*> strOption
        ( long "name"
       <> short 'n'
       <> metavar "NAME"
       <> help "Human-readable application name (e.g. \"Counter\")"
        )
  <*> strOption
        ( long "identifier"
       <> short 'i'
       <> metavar "ID"
       <> help "Bundle identifier (e.g. \"com.haskell-objc.counter\")"
        )
  <*> strOption
        ( long "output"
       <> short 'o'
       <> metavar "PATH"
       <> help "Output path for the .app bundle (e.g. \"dist/Counter.app\")"
        )
  <*> optional (strOption
        ( long "icon"
       <> metavar "PATH"
       <> help "Path to an .icns icon file"
        ))
  <*> optional (strOption
        ( long "sign"
       <> metavar "IDENTITY"
       <> help "Codesigning identity (e.g. \"Developer ID Application: ...\")"
        ))

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  cfg <- execParser $ info (bundleConfigParser <**> helper)
    ( fullDesc
   <> progDesc "Package a GHC-dynamic Haskell executable into a macOS .app bundle"
   <> header "objc-bundler - macOS app bundler for Haskell"
    )
  createBundle cfg
