{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREcosystemInformationClusterDeviceTypeStruct@.
module ObjC.Matter.MTREcosystemInformationClusterDeviceTypeStruct
  ( MTREcosystemInformationClusterDeviceTypeStruct
  , IsMTREcosystemInformationClusterDeviceTypeStruct(..)
  , deviceType
  , setDeviceType
  , revision
  , setRevision
  , deviceTypeSelector
  , setDeviceTypeSelector
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
deviceType :: IsMTREcosystemInformationClusterDeviceTypeStruct mtrEcosystemInformationClusterDeviceTypeStruct => mtrEcosystemInformationClusterDeviceTypeStruct -> IO (Id NSNumber)
deviceType mtrEcosystemInformationClusterDeviceTypeStruct  =
    sendMsg mtrEcosystemInformationClusterDeviceTypeStruct (mkSelector "deviceType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDeviceType:@
setDeviceType :: (IsMTREcosystemInformationClusterDeviceTypeStruct mtrEcosystemInformationClusterDeviceTypeStruct, IsNSNumber value) => mtrEcosystemInformationClusterDeviceTypeStruct -> value -> IO ()
setDeviceType mtrEcosystemInformationClusterDeviceTypeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEcosystemInformationClusterDeviceTypeStruct (mkSelector "setDeviceType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- revision@
revision :: IsMTREcosystemInformationClusterDeviceTypeStruct mtrEcosystemInformationClusterDeviceTypeStruct => mtrEcosystemInformationClusterDeviceTypeStruct -> IO (Id NSNumber)
revision mtrEcosystemInformationClusterDeviceTypeStruct  =
    sendMsg mtrEcosystemInformationClusterDeviceTypeStruct (mkSelector "revision") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setRevision:@
setRevision :: (IsMTREcosystemInformationClusterDeviceTypeStruct mtrEcosystemInformationClusterDeviceTypeStruct, IsNSNumber value) => mtrEcosystemInformationClusterDeviceTypeStruct -> value -> IO ()
setRevision mtrEcosystemInformationClusterDeviceTypeStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEcosystemInformationClusterDeviceTypeStruct (mkSelector "setRevision:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @deviceType@
deviceTypeSelector :: Selector
deviceTypeSelector = mkSelector "deviceType"

-- | @Selector@ for @setDeviceType:@
setDeviceTypeSelector :: Selector
setDeviceTypeSelector = mkSelector "setDeviceType:"

-- | @Selector@ for @revision@
revisionSelector :: Selector
revisionSelector = mkSelector "revision"

-- | @Selector@ for @setRevision:@
setRevisionSelector :: Selector
setRevisionSelector = mkSelector "setRevision:"

