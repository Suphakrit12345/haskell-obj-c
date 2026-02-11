{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MESampleCursorChunk
--
-- Provides information about the chunk of media where a sample is located.
--
-- An instance of this class is returned by calls to the MESampleCursor method chunkDetails.
--
-- Generated bindings for @MESampleCursorChunk@.
module ObjC.MediaExtension.MESampleCursorChunk
  ( MESampleCursorChunk
  , IsMESampleCursorChunk(..)
  , new
  , init_
  , byteSource
  , sampleIndexWithinChunk
  , newSelector
  , initSelector
  , byteSourceSelector
  , sampleIndexWithinChunkSelector


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
new :: IO (Id MESampleCursorChunk)
new  =
  do
    cls' <- getRequiredClass "MESampleCursorChunk"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMESampleCursorChunk meSampleCursorChunk => meSampleCursorChunk -> IO (Id MESampleCursorChunk)
init_ meSampleCursorChunk  =
    sendMsg meSampleCursorChunk (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | byteSource
--
-- The MEByteSource to be used to read the data for the sample.
--
-- ObjC selector: @- byteSource@
byteSource :: IsMESampleCursorChunk meSampleCursorChunk => meSampleCursorChunk -> IO (Id MEByteSource)
byteSource meSampleCursorChunk  =
    sendMsg meSampleCursorChunk (mkSelector "byteSource") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | sampleIndexWithinChunk
--
-- The offset of the sample within the chunk, in samples.
--
-- Index value 0 corresponds to the start of the chunk. You would step back this many samples to position the cursor at the start of the chunk. Subtract from the chunkInfo.chunkSampleCount field to obtain the number of samples to the end of the chunk.
--
-- ObjC selector: @- sampleIndexWithinChunk@
sampleIndexWithinChunk :: IsMESampleCursorChunk meSampleCursorChunk => meSampleCursorChunk -> IO CLong
sampleIndexWithinChunk meSampleCursorChunk  =
    sendMsg meSampleCursorChunk (mkSelector "sampleIndexWithinChunk") retCLong []

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

-- | @Selector@ for @sampleIndexWithinChunk@
sampleIndexWithinChunkSelector :: Selector
sampleIndexWithinChunkSelector = mkSelector "sampleIndexWithinChunk"

