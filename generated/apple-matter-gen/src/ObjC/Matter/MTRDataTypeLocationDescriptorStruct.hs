{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeLocationDescriptorStruct@.
module ObjC.Matter.MTRDataTypeLocationDescriptorStruct
  ( MTRDataTypeLocationDescriptorStruct
  , IsMTRDataTypeLocationDescriptorStruct(..)
  , locationName
  , setLocationName
  , floorNumber
  , setFloorNumber
  , areaType
  , setAreaType
  , locationNameSelector
  , setLocationNameSelector
  , floorNumberSelector
  , setFloorNumberSelector
  , areaTypeSelector
  , setAreaTypeSelector


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

-- | @- locationName@
locationName :: IsMTRDataTypeLocationDescriptorStruct mtrDataTypeLocationDescriptorStruct => mtrDataTypeLocationDescriptorStruct -> IO (Id NSString)
locationName mtrDataTypeLocationDescriptorStruct  =
    sendMsg mtrDataTypeLocationDescriptorStruct (mkSelector "locationName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocationName:@
setLocationName :: (IsMTRDataTypeLocationDescriptorStruct mtrDataTypeLocationDescriptorStruct, IsNSString value) => mtrDataTypeLocationDescriptorStruct -> value -> IO ()
setLocationName mtrDataTypeLocationDescriptorStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeLocationDescriptorStruct (mkSelector "setLocationName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- floorNumber@
floorNumber :: IsMTRDataTypeLocationDescriptorStruct mtrDataTypeLocationDescriptorStruct => mtrDataTypeLocationDescriptorStruct -> IO (Id NSNumber)
floorNumber mtrDataTypeLocationDescriptorStruct  =
    sendMsg mtrDataTypeLocationDescriptorStruct (mkSelector "floorNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setFloorNumber:@
setFloorNumber :: (IsMTRDataTypeLocationDescriptorStruct mtrDataTypeLocationDescriptorStruct, IsNSNumber value) => mtrDataTypeLocationDescriptorStruct -> value -> IO ()
setFloorNumber mtrDataTypeLocationDescriptorStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeLocationDescriptorStruct (mkSelector "setFloorNumber:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- areaType@
areaType :: IsMTRDataTypeLocationDescriptorStruct mtrDataTypeLocationDescriptorStruct => mtrDataTypeLocationDescriptorStruct -> IO (Id NSNumber)
areaType mtrDataTypeLocationDescriptorStruct  =
    sendMsg mtrDataTypeLocationDescriptorStruct (mkSelector "areaType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAreaType:@
setAreaType :: (IsMTRDataTypeLocationDescriptorStruct mtrDataTypeLocationDescriptorStruct, IsNSNumber value) => mtrDataTypeLocationDescriptorStruct -> value -> IO ()
setAreaType mtrDataTypeLocationDescriptorStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeLocationDescriptorStruct (mkSelector "setAreaType:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @locationName@
locationNameSelector :: Selector
locationNameSelector = mkSelector "locationName"

-- | @Selector@ for @setLocationName:@
setLocationNameSelector :: Selector
setLocationNameSelector = mkSelector "setLocationName:"

-- | @Selector@ for @floorNumber@
floorNumberSelector :: Selector
floorNumberSelector = mkSelector "floorNumber"

-- | @Selector@ for @setFloorNumber:@
setFloorNumberSelector :: Selector
setFloorNumberSelector = mkSelector "setFloorNumber:"

-- | @Selector@ for @areaType@
areaTypeSelector :: Selector
areaTypeSelector = mkSelector "areaType"

-- | @Selector@ for @setAreaType:@
setAreaTypeSelector :: Selector
setAreaTypeSelector = mkSelector "setAreaType:"

