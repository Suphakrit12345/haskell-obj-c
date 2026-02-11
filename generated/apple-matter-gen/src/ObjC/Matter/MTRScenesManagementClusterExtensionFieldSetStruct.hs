{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRScenesManagementClusterExtensionFieldSetStruct@.
module ObjC.Matter.MTRScenesManagementClusterExtensionFieldSetStruct
  ( MTRScenesManagementClusterExtensionFieldSetStruct
  , IsMTRScenesManagementClusterExtensionFieldSetStruct(..)
  , clusterID
  , setClusterID
  , attributeValueList
  , setAttributeValueList
  , clusterIDSelector
  , setClusterIDSelector
  , attributeValueListSelector
  , setAttributeValueListSelector


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

-- | @- clusterID@
clusterID :: IsMTRScenesManagementClusterExtensionFieldSetStruct mtrScenesManagementClusterExtensionFieldSetStruct => mtrScenesManagementClusterExtensionFieldSetStruct -> IO (Id NSNumber)
clusterID mtrScenesManagementClusterExtensionFieldSetStruct  =
    sendMsg mtrScenesManagementClusterExtensionFieldSetStruct (mkSelector "clusterID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setClusterID:@
setClusterID :: (IsMTRScenesManagementClusterExtensionFieldSetStruct mtrScenesManagementClusterExtensionFieldSetStruct, IsNSNumber value) => mtrScenesManagementClusterExtensionFieldSetStruct -> value -> IO ()
setClusterID mtrScenesManagementClusterExtensionFieldSetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterExtensionFieldSetStruct (mkSelector "setClusterID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- attributeValueList@
attributeValueList :: IsMTRScenesManagementClusterExtensionFieldSetStruct mtrScenesManagementClusterExtensionFieldSetStruct => mtrScenesManagementClusterExtensionFieldSetStruct -> IO (Id NSArray)
attributeValueList mtrScenesManagementClusterExtensionFieldSetStruct  =
    sendMsg mtrScenesManagementClusterExtensionFieldSetStruct (mkSelector "attributeValueList") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAttributeValueList:@
setAttributeValueList :: (IsMTRScenesManagementClusterExtensionFieldSetStruct mtrScenesManagementClusterExtensionFieldSetStruct, IsNSArray value) => mtrScenesManagementClusterExtensionFieldSetStruct -> value -> IO ()
setAttributeValueList mtrScenesManagementClusterExtensionFieldSetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrScenesManagementClusterExtensionFieldSetStruct (mkSelector "setAttributeValueList:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @clusterID@
clusterIDSelector :: Selector
clusterIDSelector = mkSelector "clusterID"

-- | @Selector@ for @setClusterID:@
setClusterIDSelector :: Selector
setClusterIDSelector = mkSelector "setClusterID:"

-- | @Selector@ for @attributeValueList@
attributeValueListSelector :: Selector
attributeValueListSelector = mkSelector "attributeValueList"

-- | @Selector@ for @setAttributeValueList:@
setAttributeValueListSelector :: Selector
setAttributeValueListSelector = mkSelector "setAttributeValueList:"

