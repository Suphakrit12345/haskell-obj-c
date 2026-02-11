{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterSupportedFormatStruct@.
module ObjC.Matter.MTRPushAVStreamTransportClusterSupportedFormatStruct
  ( MTRPushAVStreamTransportClusterSupportedFormatStruct
  , IsMTRPushAVStreamTransportClusterSupportedFormatStruct(..)
  , containerFormat
  , setContainerFormat
  , ingestMethod
  , setIngestMethod
  , containerFormatSelector
  , setContainerFormatSelector
  , ingestMethodSelector
  , setIngestMethodSelector


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

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- containerFormat@
containerFormat :: IsMTRPushAVStreamTransportClusterSupportedFormatStruct mtrPushAVStreamTransportClusterSupportedFormatStruct => mtrPushAVStreamTransportClusterSupportedFormatStruct -> IO (Id NSNumber)
containerFormat mtrPushAVStreamTransportClusterSupportedFormatStruct  =
    sendMsg mtrPushAVStreamTransportClusterSupportedFormatStruct (mkSelector "containerFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContainerFormat:@
setContainerFormat :: (IsMTRPushAVStreamTransportClusterSupportedFormatStruct mtrPushAVStreamTransportClusterSupportedFormatStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterSupportedFormatStruct -> value -> IO ()
setContainerFormat mtrPushAVStreamTransportClusterSupportedFormatStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterSupportedFormatStruct (mkSelector "setContainerFormat:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- ingestMethod@
ingestMethod :: IsMTRPushAVStreamTransportClusterSupportedFormatStruct mtrPushAVStreamTransportClusterSupportedFormatStruct => mtrPushAVStreamTransportClusterSupportedFormatStruct -> IO (Id NSNumber)
ingestMethod mtrPushAVStreamTransportClusterSupportedFormatStruct  =
    sendMsg mtrPushAVStreamTransportClusterSupportedFormatStruct (mkSelector "ingestMethod") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIngestMethod:@
setIngestMethod :: (IsMTRPushAVStreamTransportClusterSupportedFormatStruct mtrPushAVStreamTransportClusterSupportedFormatStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterSupportedFormatStruct -> value -> IO ()
setIngestMethod mtrPushAVStreamTransportClusterSupportedFormatStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterSupportedFormatStruct (mkSelector "setIngestMethod:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @containerFormat@
containerFormatSelector :: Selector
containerFormatSelector = mkSelector "containerFormat"

-- | @Selector@ for @setContainerFormat:@
setContainerFormatSelector :: Selector
setContainerFormatSelector = mkSelector "setContainerFormat:"

-- | @Selector@ for @ingestMethod@
ingestMethodSelector :: Selector
ingestMethodSelector = mkSelector "ingestMethod"

-- | @Selector@ for @setIngestMethod:@
setIngestMethodSelector :: Selector
setIngestMethodSelector = mkSelector "setIngestMethod:"

