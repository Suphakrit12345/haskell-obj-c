{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A range that describes contiguous metadata segments on disk.
--
-- This type represents a range that begins at @startOffset@ and ends at @startOffset + segmentLength * segmentCount@. Each segment in the range represents a single block in the resource's buffer cache.
--
-- For example, given an @FSMetadataRange@ with the following properties:
--
-- * @startOffset = 0@ * @segmentLength = 512@ * @segmentCount = 8@
--
-- The range represents eight segments: from 0 to 511, then from 512 to 1023, and so on until a final segment of 3584 to 4095.
--
-- Ensure that each metadata segment represents a range that's already present in the resource's buffer cache. Similarly, ensure that each segment's offset and length matches the offset and length of the corresponding block in the buffer cache.
--
-- Generated bindings for @FSMetadataRange@.
module ObjC.FSKit.FSMetadataRange
  ( FSMetadataRange
  , IsFSMetadataRange(..)
  , initWithOffset_segmentLength_segmentCount
  , rangeWithOffset_segmentLength_segmentCount
  , init_
  , startOffset
  , segmentLength
  , segmentCount
  , initWithOffset_segmentLength_segmentCountSelector
  , rangeWithOffset_segmentLength_segmentCountSelector
  , initSelector
  , startOffsetSelector
  , segmentLengthSelector
  , segmentCountSelector


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

import ObjC.FSKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a metadata range with the given properties.
--
-- - Parameters:   - startOffset: The start offset of the range in bytes. Ensure this value is a multiple of the corresponding resource's ``FSBlockDeviceResource-c.class/blockSize``.   - segmentLength: The segment length in bytes. Ensure this value is a multiple of the corresponding resource's ``FSBlockDeviceResource-c.class/blockSize``.   - segmentCount: The number of segments in the range.
--
-- ObjC selector: @- initWithOffset:segmentLength:segmentCount:@
initWithOffset_segmentLength_segmentCount :: IsFSMetadataRange fsMetadataRange => fsMetadataRange -> CLong -> CULong -> CULong -> IO (Id FSMetadataRange)
initWithOffset_segmentLength_segmentCount fsMetadataRange  startOffset segmentLength segmentCount =
    sendMsg fsMetadataRange (mkSelector "initWithOffset:segmentLength:segmentCount:") (retPtr retVoid) [argCLong startOffset, argCULong segmentLength, argCULong segmentCount] >>= ownedObject . castPtr

-- | Creates a metadata range with the given properties. - Parameters:   - startOffset: The start offset of the range in bytes. Ensure this value is a multiple of the corresponding resource's ``FSBlockDeviceResource-c.class/blockSize``.   - segmentLength: The segment length in bytes. Ensure this value is a multiple of the corresponding resource's ``FSBlockDeviceResource-c.class/blockSize``.   - segmentCount: The number of segments in the range.
--
-- ObjC selector: @+ rangeWithOffset:segmentLength:segmentCount:@
rangeWithOffset_segmentLength_segmentCount :: CLong -> CULong -> CULong -> IO (Id FSMetadataRange)
rangeWithOffset_segmentLength_segmentCount startOffset segmentLength segmentCount =
  do
    cls' <- getRequiredClass "FSMetadataRange"
    sendClassMsg cls' (mkSelector "rangeWithOffset:segmentLength:segmentCount:") (retPtr retVoid) [argCLong startOffset, argCULong segmentLength, argCULong segmentCount] >>= retainedObject . castPtr

-- | @- init@
init_ :: IsFSMetadataRange fsMetadataRange => fsMetadataRange -> IO (Id FSMetadataRange)
init_ fsMetadataRange  =
    sendMsg fsMetadataRange (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The start offset of the range in bytes.
--
-- Ensure this value is a multiple of the corresponding resource's ``FSBlockDeviceResource-c.class/blockSize``.
--
-- ObjC selector: @- startOffset@
startOffset :: IsFSMetadataRange fsMetadataRange => fsMetadataRange -> IO CLong
startOffset fsMetadataRange  =
    sendMsg fsMetadataRange (mkSelector "startOffset") retCLong []

-- | The segment length in bytes.
--
-- Ensure this value is a multiple of the corresponding resource's ``FSBlockDeviceResource-c.class/blockSize``.
--
-- ObjC selector: @- segmentLength@
segmentLength :: IsFSMetadataRange fsMetadataRange => fsMetadataRange -> IO CULong
segmentLength fsMetadataRange  =
    sendMsg fsMetadataRange (mkSelector "segmentLength") retCULong []

-- | The number of segments in the range.
--
-- ObjC selector: @- segmentCount@
segmentCount :: IsFSMetadataRange fsMetadataRange => fsMetadataRange -> IO CULong
segmentCount fsMetadataRange  =
    sendMsg fsMetadataRange (mkSelector "segmentCount") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithOffset:segmentLength:segmentCount:@
initWithOffset_segmentLength_segmentCountSelector :: Selector
initWithOffset_segmentLength_segmentCountSelector = mkSelector "initWithOffset:segmentLength:segmentCount:"

-- | @Selector@ for @rangeWithOffset:segmentLength:segmentCount:@
rangeWithOffset_segmentLength_segmentCountSelector :: Selector
rangeWithOffset_segmentLength_segmentCountSelector = mkSelector "rangeWithOffset:segmentLength:segmentCount:"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @startOffset@
startOffsetSelector :: Selector
startOffsetSelector = mkSelector "startOffset"

-- | @Selector@ for @segmentLength@
segmentLengthSelector :: Selector
segmentLengthSelector = mkSelector "segmentLength"

-- | @Selector@ for @segmentCount@
segmentCountSelector :: Selector
segmentCountSelector = mkSelector "segmentCount"

