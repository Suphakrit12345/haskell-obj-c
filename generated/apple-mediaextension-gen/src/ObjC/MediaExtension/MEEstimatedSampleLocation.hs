{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MEEstimatedSampleLocation
--
-- Provides information about the estimated sample location with the media.
--
-- An instance of this class is returned by calls to the MESampleCursor method estimatedSampleLocationReturningError.
--
-- Generated bindings for @MEEstimatedSampleLocation@.
module ObjC.MediaExtension.MEEstimatedSampleLocation
  ( MEEstimatedSampleLocation
  , IsMEEstimatedSampleLocation(..)
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
new :: IO (Id MEEstimatedSampleLocation)
new  =
  do
    cls' <- getRequiredClass "MEEstimatedSampleLocation"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMEEstimatedSampleLocation meEstimatedSampleLocation => meEstimatedSampleLocation -> IO (Id MEEstimatedSampleLocation)
init_ meEstimatedSampleLocation  =
    sendMsg meEstimatedSampleLocation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | byteSource
--
-- The MEByteSource to be used to read the data for the sample.
--
-- ObjC selector: @- byteSource@
byteSource :: IsMEEstimatedSampleLocation meEstimatedSampleLocation => meEstimatedSampleLocation -> IO (Id MEByteSource)
byteSource meEstimatedSampleLocation  =
    sendMsg meEstimatedSampleLocation (mkSelector "byteSource") (retPtr retVoid) [] >>= retainedObject . castPtr

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

