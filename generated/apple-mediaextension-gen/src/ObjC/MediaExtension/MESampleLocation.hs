{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MESampleLocation
--
-- Provides information about the sample location with the media.
--
-- An instance of this class is returned by calls to the MESampleCursor method sampleLocation.
--
-- Generated bindings for @MESampleLocation@.
module ObjC.MediaExtension.MESampleLocation
  ( MESampleLocation
  , IsMESampleLocation(..)
  , new
  , init_
  , byteSource
  , newSelector
  , initSelector
  , byteSourceSelector


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

import ObjC.MediaExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MESampleLocation)
new  =
  do
    cls' <- getRequiredClass "MESampleLocation"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMESampleLocation meSampleLocation => meSampleLocation -> IO (Id MESampleLocation)
init_ meSampleLocation  =
    sendMsg meSampleLocation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | byteSource
--
-- The MEByteSource to be used to read the data for the sample.
--
-- ObjC selector: @- byteSource@
byteSource :: IsMESampleLocation meSampleLocation => meSampleLocation -> IO (Id MEByteSource)
byteSource meSampleLocation  =
    sendMsg meSampleLocation (mkSelector "byteSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @byteSource@
byteSourceSelector :: Selector
byteSourceSelector = mkSelector "byteSource"

