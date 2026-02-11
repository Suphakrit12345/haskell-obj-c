{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRServiceAreaClusterProgressStruct@.
module ObjC.Matter.MTRServiceAreaClusterProgressStruct
  ( MTRServiceAreaClusterProgressStruct
  , IsMTRServiceAreaClusterProgressStruct(..)
  , areaID
  , setAreaID
  , status
  , setStatus
  , totalOperationalTime
  , setTotalOperationalTime
  , estimatedTime
  , setEstimatedTime
  , areaIDSelector
  , setAreaIDSelector
  , statusSelector
  , setStatusSelector
  , totalOperationalTimeSelector
  , setTotalOperationalTimeSelector
  , estimatedTimeSelector
  , setEstimatedTimeSelector


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

-- | @- areaID@
areaID :: IsMTRServiceAreaClusterProgressStruct mtrServiceAreaClusterProgressStruct => mtrServiceAreaClusterProgressStruct -> IO (Id NSNumber)
areaID mtrServiceAreaClusterProgressStruct  =
    sendMsg mtrServiceAreaClusterProgressStruct (mkSelector "areaID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAreaID:@
setAreaID :: (IsMTRServiceAreaClusterProgressStruct mtrServiceAreaClusterProgressStruct, IsNSNumber value) => mtrServiceAreaClusterProgressStruct -> value -> IO ()
setAreaID mtrServiceAreaClusterProgressStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrServiceAreaClusterProgressStruct (mkSelector "setAreaID:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- status@
status :: IsMTRServiceAreaClusterProgressStruct mtrServiceAreaClusterProgressStruct => mtrServiceAreaClusterProgressStruct -> IO (Id NSNumber)
status mtrServiceAreaClusterProgressStruct  =
    sendMsg mtrServiceAreaClusterProgressStruct (mkSelector "status") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setStatus:@
setStatus :: (IsMTRServiceAreaClusterProgressStruct mtrServiceAreaClusterProgressStruct, IsNSNumber value) => mtrServiceAreaClusterProgressStruct -> value -> IO ()
setStatus mtrServiceAreaClusterProgressStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrServiceAreaClusterProgressStruct (mkSelector "setStatus:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- totalOperationalTime@
totalOperationalTime :: IsMTRServiceAreaClusterProgressStruct mtrServiceAreaClusterProgressStruct => mtrServiceAreaClusterProgressStruct -> IO (Id NSNumber)
totalOperationalTime mtrServiceAreaClusterProgressStruct  =
    sendMsg mtrServiceAreaClusterProgressStruct (mkSelector "totalOperationalTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTotalOperationalTime:@
setTotalOperationalTime :: (IsMTRServiceAreaClusterProgressStruct mtrServiceAreaClusterProgressStruct, IsNSNumber value) => mtrServiceAreaClusterProgressStruct -> value -> IO ()
setTotalOperationalTime mtrServiceAreaClusterProgressStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrServiceAreaClusterProgressStruct (mkSelector "setTotalOperationalTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- estimatedTime@
estimatedTime :: IsMTRServiceAreaClusterProgressStruct mtrServiceAreaClusterProgressStruct => mtrServiceAreaClusterProgressStruct -> IO (Id NSNumber)
estimatedTime mtrServiceAreaClusterProgressStruct  =
    sendMsg mtrServiceAreaClusterProgressStruct (mkSelector "estimatedTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEstimatedTime:@
setEstimatedTime :: (IsMTRServiceAreaClusterProgressStruct mtrServiceAreaClusterProgressStruct, IsNSNumber value) => mtrServiceAreaClusterProgressStruct -> value -> IO ()
setEstimatedTime mtrServiceAreaClusterProgressStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrServiceAreaClusterProgressStruct (mkSelector "setEstimatedTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @areaID@
areaIDSelector :: Selector
areaIDSelector = mkSelector "areaID"

-- | @Selector@ for @setAreaID:@
setAreaIDSelector :: Selector
setAreaIDSelector = mkSelector "setAreaID:"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @setStatus:@
setStatusSelector :: Selector
setStatusSelector = mkSelector "setStatus:"

-- | @Selector@ for @totalOperationalTime@
totalOperationalTimeSelector :: Selector
totalOperationalTimeSelector = mkSelector "totalOperationalTime"

-- | @Selector@ for @setTotalOperationalTime:@
setTotalOperationalTimeSelector :: Selector
setTotalOperationalTimeSelector = mkSelector "setTotalOperationalTime:"

-- | @Selector@ for @estimatedTime@
estimatedTimeSelector :: Selector
estimatedTimeSelector = mkSelector "estimatedTime"

-- | @Selector@ for @setEstimatedTime:@
setEstimatedTimeSelector :: Selector
setEstimatedTimeSelector = mkSelector "setEstimatedTime:"

