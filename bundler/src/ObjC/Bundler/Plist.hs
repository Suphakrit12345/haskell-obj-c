{-# LANGUAGE OverloadedStrings #-}

-- | Typed plist DSL and xml-conduit rendering for Info.plist generation.
--
-- Builds proper Apple XML plists using @xml-conduit@ so all values are
-- correctly escaped and the DOCTYPE declaration is included.
module ObjC.Bundler.Plist
  ( PlistValue(..)
  , renderPlist
  ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.XML
  ( Document(..)
  , Element(..)
  , Name(..)
  , Node(..)
  , Prologue(..)
  , Doctype(..)
  , ExternalID(..)
  , def
  , renderLBS
  )

-- | A property list value.
data PlistValue
  = PlistString !Text
  | PlistBool !Bool
  | PlistInt !Int
  | PlistArray ![PlistValue]
  | PlistDict ![(Text, PlistValue)]
  deriving (Show, Eq)

-- | Render a plist dictionary to a complete XML plist document.
--
-- Produces output like:
--
-- > <?xml version="1.0" encoding="UTF-8"?>
-- > <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
-- > <plist version="1.0">
-- >   <dict>
-- >     <key>CFBundleName</key>
-- >     <string>MyApp</string>
-- >     ...
-- >   </dict>
-- > </plist>
renderPlist :: [(Text, PlistValue)] -> LBS.ByteString
renderPlist entries = renderLBS settings doc
  where
    settings = def

    doc = Document
      { documentPrologue = prologue
      , documentRoot     = rootElement
      , documentEpilogue = []
      }

    prologue = Prologue
      { prologueBefore = []
      , prologueDoctype = Just Doctype
          { doctypeName = "plist"
          , doctypeID = Just (PublicID
              "-//Apple//DTD PLIST 1.0//EN"
              "http://www.apple.com/DTDs/PropertyList-1.0.dtd")
          }
      , prologueAfter = []
      }

    rootElement = Element
      { elementName = "plist"
      , elementAttributes = Map.singleton "version" "1.0"
      , elementNodes = [NodeElement (dictElement entries)]
      }

-- | Convert a list of key-value pairs to a @\<dict\>@ element.
dictElement :: [(Text, PlistValue)] -> Element
dictElement pairs = Element
  { elementName = "dict"
  , elementAttributes = Map.empty
  , elementNodes = concatMap pairToNodes pairs
  }
  where
    pairToNodes (k, v) =
      [ NodeElement (textElement "key" k)
      , NodeElement (valueToElement v)
      ]

-- | Convert a 'PlistValue' to an XML 'Element'.
valueToElement :: PlistValue -> Element
valueToElement (PlistString t) = textElement "string" t
valueToElement (PlistBool True) = emptyElement "true"
valueToElement (PlistBool False) = emptyElement "false"
valueToElement (PlistInt n) = textElement "integer" (T.pack (show n))
valueToElement (PlistArray vs) = Element
  { elementName = "array"
  , elementAttributes = Map.empty
  , elementNodes = map (NodeElement . valueToElement) vs
  }
valueToElement (PlistDict pairs) = dictElement pairs

-- | An element with a single text child node.
textElement :: Name -> Text -> Element
textElement name content = Element
  { elementName = name
  , elementAttributes = Map.empty
  , elementNodes = [NodeContent content]
  }

-- | An element with no children (self-closing, e.g. @\<true/\>@).
emptyElement :: Name -> Element
emptyElement name = Element
  { elementName = name
  , elementAttributes = Map.empty
  , elementNodes = []
  }
