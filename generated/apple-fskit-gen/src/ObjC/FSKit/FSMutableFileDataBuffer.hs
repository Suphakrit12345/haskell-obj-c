{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A wrapper object for a data buffer.
--
-- This object provides a "zero-copy" buffer, for use when reading data from files. By not requiring additional buffer copying, this object reduces the extension's memory footprint and improves performance. The @FSMutableFileDataBuffer@ behaves similarly to a @uio@ in the kernel.
--
-- Generated bindings for @FSMutableFileDataBuffer@.
module ObjC.FSKit.FSMutableFileDataBuffer
  ( FSMutableFileDataBuffer
  , IsFSMutableFileDataBuffer(..)
  , init_
  , mutableBytes
  , length_
  , initSelector
  , mutableBytesSelector
  , lengthSelector


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

-- | @- init@
init_ :: IsFSMutableFileDataBuffer fsMutableFileDataBuffer => fsMutableFileDataBuffer -> IO (Id FSMutableFileDataBuffer)
init_ fsMutableFileDataBuffer  =
    sendMsg fsMutableFileDataBuffer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The byte data.
--
-- ObjC selector: @- mutableBytes@
mutableBytes :: IsFSMutableFileDataBuffer fsMutableFileDataBuffer => fsMutableFileDataBuffer -> IO (Ptr ())
mutableBytes fsMutableFileDataBuffer  =
    fmap castPtr $ sendMsg fsMutableFileDataBuffer (mkSelector "mutableBytes") (retPtr retVoid) []

-- | The data length of the buffer.
--
-- ObjC selector: @- length@
length_ :: IsFSMutableFileDataBuffer fsMutableFileDataBuffer => fsMutableFileDataBuffer -> IO CULong
length_ fsMutableFileDataBuffer  =
    sendMsg fsMutableFileDataBuffer (mkSelector "length") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @mutableBytes@
mutableBytesSelector :: Selector
mutableBytesSelector = mkSelector "mutableBytes"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

