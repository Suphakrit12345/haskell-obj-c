{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDescriptorClusterDeviceTypeStruct@.
module ObjC.Matter.MTRDescriptorClusterDeviceTypeStruct
  ( MTRDescriptorClusterDeviceTypeStruct
  , IsMTRDescriptorClusterDeviceTypeStruct(..)
  , deviceType
  , setDeviceType
  , type_
  , setType
  , revision
  , setRevision
  , deviceTypeSelector
  , setDeviceTypeSelector
  , typeSelector
  , setTypeSelector
  , revisionSelector
  , setRevisionSelector


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

-- | @- deviceType@
deviceType :: IsMTRDescriptorClusterDeviceTypeStruct mtrDescriptorClusterDeviceTypeStruct => mtrDescriptorClusterDeviceTypeStruct -> IO (Id NSNumber)
deviceType mtrDescriptorClusterDeviceTypeStruct  =
    sendMsg mtrDescriptorClusterDeviceTypeStruct (mkSelector "deviceType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeviceType:@
setDeviceType :: (IsMTRDescriptorClusterDeviceTypeStruct mtrDescriptorClusterDeviceTypeStruct, IsNSNumber value) => mtrDescriptorClusterDeviceTypeStruct -> value -> IO ()
setDeviceType mtrDescriptorClusterDeviceTypeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDescriptorClusterDeviceTypeStruct (mkSelector "setDeviceType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- type@
type_ :: IsMTRDescriptorClusterDeviceTypeStruct mtrDescriptorClusterDeviceTypeStruct => mtrDescriptorClusterDeviceTypeStruct -> IO (Id NSNumber)
type_ mtrDescriptorClusterDeviceTypeStruct  =
    sendMsg mtrDescriptorClusterDeviceTypeStruct (mkSelector "type") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setType:@
setType :: (IsMTRDescriptorClusterDeviceTypeStruct mtrDescriptorClusterDeviceTypeStruct, IsNSNumber value) => mtrDescriptorClusterDeviceTypeStruct -> value -> IO ()
setType mtrDescriptorClusterDeviceTypeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDescriptorClusterDeviceTypeStruct (mkSelector "setType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- revision@
revision :: IsMTRDescriptorClusterDeviceTypeStruct mtrDescriptorClusterDeviceTypeStruct => mtrDescriptorClusterDeviceTypeStruct -> IO (Id NSNumber)
revision mtrDescriptorClusterDeviceTypeStruct  =
    sendMsg mtrDescriptorClusterDeviceTypeStruct (mkSelector "revision") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRevision:@
setRevision :: (IsMTRDescriptorClusterDeviceTypeStruct mtrDescriptorClusterDeviceTypeStruct, IsNSNumber value) => mtrDescriptorClusterDeviceTypeStruct -> value -> IO ()
setRevision mtrDescriptorClusterDeviceTypeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDescriptorClusterDeviceTypeStruct (mkSelector "setRevision:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deviceType@
deviceTypeSelector :: Selector
deviceTypeSelector = mkSelector "deviceType"

-- | @Selector@ for @setDeviceType:@
setDeviceTypeSelector :: Selector
setDeviceTypeSelector = mkSelector "setDeviceType:"

-- | @Selector@ for @type@
typeSelector :: Selector
typeSelector = mkSelector "type"

-- | @Selector@ for @setType:@
setTypeSelector :: Selector
setTypeSelector = mkSelector "setType:"

-- | @Selector@ for @revision@
revisionSelector :: Selector
revisionSelector = mkSelector "revision"

-- | @Selector@ for @setRevision:@
setRevisionSelector :: Selector
setRevisionSelector = mkSelector "setRevision:"

