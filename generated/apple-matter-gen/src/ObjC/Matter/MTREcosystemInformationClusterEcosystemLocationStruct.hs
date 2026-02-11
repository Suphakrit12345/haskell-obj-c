{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTREcosystemInformationClusterEcosystemLocationStruct@.
module ObjC.Matter.MTREcosystemInformationClusterEcosystemLocationStruct
  ( MTREcosystemInformationClusterEcosystemLocationStruct
  , IsMTREcosystemInformationClusterEcosystemLocationStruct(..)
  , uniqueLocationID
  , setUniqueLocationID
  , locationDescriptor
  , setLocationDescriptor
  , locationDescriptorLastEdit
  , setLocationDescriptorLastEdit
  , fabricIndex
  , setFabricIndex
  , uniqueLocationIDSelector
  , setUniqueLocationIDSelector
  , locationDescriptorSelector
  , setLocationDescriptorSelector
  , locationDescriptorLastEditSelector
  , setLocationDescriptorLastEditSelector
  , fabricIndexSelector
  , setFabricIndexSelector


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

-- | @- uniqueLocationID@
uniqueLocationID :: IsMTREcosystemInformationClusterEcosystemLocationStruct mtrEcosystemInformationClusterEcosystemLocationStruct => mtrEcosystemInformationClusterEcosystemLocationStruct -> IO (Id NSString)
uniqueLocationID mtrEcosystemInformationClusterEcosystemLocationStruct  =
    sendMsg mtrEcosystemInformationClusterEcosystemLocationStruct (mkSelector "uniqueLocationID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUniqueLocationID:@
setUniqueLocationID :: (IsMTREcosystemInformationClusterEcosystemLocationStruct mtrEcosystemInformationClusterEcosystemLocationStruct, IsNSString value) => mtrEcosystemInformationClusterEcosystemLocationStruct -> value -> IO ()
setUniqueLocationID mtrEcosystemInformationClusterEcosystemLocationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEcosystemInformationClusterEcosystemLocationStruct (mkSelector "setUniqueLocationID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- locationDescriptor@
locationDescriptor :: IsMTREcosystemInformationClusterEcosystemLocationStruct mtrEcosystemInformationClusterEcosystemLocationStruct => mtrEcosystemInformationClusterEcosystemLocationStruct -> IO (Id MTRDataTypeLocationDescriptorStruct)
locationDescriptor mtrEcosystemInformationClusterEcosystemLocationStruct  =
    sendMsg mtrEcosystemInformationClusterEcosystemLocationStruct (mkSelector "locationDescriptor") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocationDescriptor:@
setLocationDescriptor :: (IsMTREcosystemInformationClusterEcosystemLocationStruct mtrEcosystemInformationClusterEcosystemLocationStruct, IsMTRDataTypeLocationDescriptorStruct value) => mtrEcosystemInformationClusterEcosystemLocationStruct -> value -> IO ()
setLocationDescriptor mtrEcosystemInformationClusterEcosystemLocationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEcosystemInformationClusterEcosystemLocationStruct (mkSelector "setLocationDescriptor:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- locationDescriptorLastEdit@
locationDescriptorLastEdit :: IsMTREcosystemInformationClusterEcosystemLocationStruct mtrEcosystemInformationClusterEcosystemLocationStruct => mtrEcosystemInformationClusterEcosystemLocationStruct -> IO (Id NSNumber)
locationDescriptorLastEdit mtrEcosystemInformationClusterEcosystemLocationStruct  =
    sendMsg mtrEcosystemInformationClusterEcosystemLocationStruct (mkSelector "locationDescriptorLastEdit") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocationDescriptorLastEdit:@
setLocationDescriptorLastEdit :: (IsMTREcosystemInformationClusterEcosystemLocationStruct mtrEcosystemInformationClusterEcosystemLocationStruct, IsNSNumber value) => mtrEcosystemInformationClusterEcosystemLocationStruct -> value -> IO ()
setLocationDescriptorLastEdit mtrEcosystemInformationClusterEcosystemLocationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEcosystemInformationClusterEcosystemLocationStruct (mkSelector "setLocationDescriptorLastEdit:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- fabricIndex@
fabricIndex :: IsMTREcosystemInformationClusterEcosystemLocationStruct mtrEcosystemInformationClusterEcosystemLocationStruct => mtrEcosystemInformationClusterEcosystemLocationStruct -> IO (Id NSNumber)
fabricIndex mtrEcosystemInformationClusterEcosystemLocationStruct  =
    sendMsg mtrEcosystemInformationClusterEcosystemLocationStruct (mkSelector "fabricIndex") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFabricIndex:@
setFabricIndex :: (IsMTREcosystemInformationClusterEcosystemLocationStruct mtrEcosystemInformationClusterEcosystemLocationStruct, IsNSNumber value) => mtrEcosystemInformationClusterEcosystemLocationStruct -> value -> IO ()
setFabricIndex mtrEcosystemInformationClusterEcosystemLocationStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrEcosystemInformationClusterEcosystemLocationStruct (mkSelector "setFabricIndex:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @uniqueLocationID@
uniqueLocationIDSelector :: Selector
uniqueLocationIDSelector = mkSelector "uniqueLocationID"

-- | @Selector@ for @setUniqueLocationID:@
setUniqueLocationIDSelector :: Selector
setUniqueLocationIDSelector = mkSelector "setUniqueLocationID:"

-- | @Selector@ for @locationDescriptor@
locationDescriptorSelector :: Selector
locationDescriptorSelector = mkSelector "locationDescriptor"

-- | @Selector@ for @setLocationDescriptor:@
setLocationDescriptorSelector :: Selector
setLocationDescriptorSelector = mkSelector "setLocationDescriptor:"

-- | @Selector@ for @locationDescriptorLastEdit@
locationDescriptorLastEditSelector :: Selector
locationDescriptorLastEditSelector = mkSelector "locationDescriptorLastEdit"

-- | @Selector@ for @setLocationDescriptorLastEdit:@
setLocationDescriptorLastEditSelector :: Selector
setLocationDescriptorLastEditSelector = mkSelector "setLocationDescriptorLastEdit:"

-- | @Selector@ for @fabricIndex@
fabricIndexSelector :: Selector
fabricIndexSelector = mkSelector "fabricIndex"

-- | @Selector@ for @setFabricIndex:@
setFabricIndexSelector :: Selector
setFabricIndexSelector = mkSelector "setFabricIndex:"

