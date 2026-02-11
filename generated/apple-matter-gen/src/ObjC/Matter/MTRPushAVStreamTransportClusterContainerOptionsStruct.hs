{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRPushAVStreamTransportClusterContainerOptionsStruct@.
module ObjC.Matter.MTRPushAVStreamTransportClusterContainerOptionsStruct
  ( MTRPushAVStreamTransportClusterContainerOptionsStruct
  , IsMTRPushAVStreamTransportClusterContainerOptionsStruct(..)
  , containerType
  , setContainerType
  , cmafContainerOptions
  , setCmafContainerOptions
  , containerTypeSelector
  , setContainerTypeSelector
  , cmafContainerOptionsSelector
  , setCmafContainerOptionsSelector


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

-- | @- containerType@
containerType :: IsMTRPushAVStreamTransportClusterContainerOptionsStruct mtrPushAVStreamTransportClusterContainerOptionsStruct => mtrPushAVStreamTransportClusterContainerOptionsStruct -> IO (Id NSNumber)
containerType mtrPushAVStreamTransportClusterContainerOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterContainerOptionsStruct (mkSelector "containerType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setContainerType:@
setContainerType :: (IsMTRPushAVStreamTransportClusterContainerOptionsStruct mtrPushAVStreamTransportClusterContainerOptionsStruct, IsNSNumber value) => mtrPushAVStreamTransportClusterContainerOptionsStruct -> value -> IO ()
setContainerType mtrPushAVStreamTransportClusterContainerOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterContainerOptionsStruct (mkSelector "setContainerType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- cmafContainerOptions@
cmafContainerOptions :: IsMTRPushAVStreamTransportClusterContainerOptionsStruct mtrPushAVStreamTransportClusterContainerOptionsStruct => mtrPushAVStreamTransportClusterContainerOptionsStruct -> IO (Id MTRPushAVStreamTransportClusterCMAFContainerOptionsStruct)
cmafContainerOptions mtrPushAVStreamTransportClusterContainerOptionsStruct  =
    sendMsg mtrPushAVStreamTransportClusterContainerOptionsStruct (mkSelector "cmafContainerOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCmafContainerOptions:@
setCmafContainerOptions :: (IsMTRPushAVStreamTransportClusterContainerOptionsStruct mtrPushAVStreamTransportClusterContainerOptionsStruct, IsMTRPushAVStreamTransportClusterCMAFContainerOptionsStruct value) => mtrPushAVStreamTransportClusterContainerOptionsStruct -> value -> IO ()
setCmafContainerOptions mtrPushAVStreamTransportClusterContainerOptionsStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrPushAVStreamTransportClusterContainerOptionsStruct (mkSelector "setCmafContainerOptions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @containerType@
containerTypeSelector :: Selector
containerTypeSelector = mkSelector "containerType"

-- | @Selector@ for @setContainerType:@
setContainerTypeSelector :: Selector
setContainerTypeSelector = mkSelector "setContainerType:"

-- | @Selector@ for @cmafContainerOptions@
cmafContainerOptionsSelector :: Selector
cmafContainerOptionsSelector = mkSelector "cmafContainerOptions"

-- | @Selector@ for @setCmafContainerOptions:@
setCmafContainerOptionsSelector :: Selector
setCmafContainerOptionsSelector = mkSelector "setCmafContainerOptions:"

