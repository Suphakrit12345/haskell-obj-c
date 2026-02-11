{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The ITLibArtwork class represents a media item artwork.
--
-- Generated bindings for @ITLibArtwork@.
module ObjC.ITunesLibrary.ITLibArtwork
  ( ITLibArtwork
  , IsITLibArtwork(..)
  , imageData
  , imageDataFormat
  , imageDataSelector
  , imageDataFormatSelector

  -- * Enum types
  , ITLibArtworkFormat(ITLibArtworkFormat)
  , pattern ITLibArtworkFormatNone
  , pattern ITLibArtworkFormatBitmap
  , pattern ITLibArtworkFormatJPEG
  , pattern ITLibArtworkFormatJPEG2000
  , pattern ITLibArtworkFormatGIF
  , pattern ITLibArtworkFormatPNG
  , pattern ITLibArtworkFormatBMP
  , pattern ITLibArtworkFormatTIFF
  , pattern ITLibArtworkFormatPICT

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ITunesLibrary.Internal.Classes
import ObjC.ITunesLibrary.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | The data (bytes) of this artwork image.
--
-- ObjC selector: @- imageData@
imageData :: IsITLibArtwork itLibArtwork => itLibArtwork -> IO (Id NSData)
imageData itLibArtwork  =
    sendMsg itLibArtwork (mkSelector "imageData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The fortmat of the data returned by the imageData method.
--
-- ObjC selector: @- imageDataFormat@
imageDataFormat :: IsITLibArtwork itLibArtwork => itLibArtwork -> IO ITLibArtworkFormat
imageDataFormat itLibArtwork  =
    fmap (coerce :: CULong -> ITLibArtworkFormat) $ sendMsg itLibArtwork (mkSelector "imageDataFormat") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @imageData@
imageDataSelector :: Selector
imageDataSelector = mkSelector "imageData"

-- | @Selector@ for @imageDataFormat@
imageDataFormatSelector :: Selector
imageDataFormatSelector = mkSelector "imageDataFormat"

