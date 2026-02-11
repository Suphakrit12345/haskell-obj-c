{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterTimeZoneStruct@.
module ObjC.Matter.MTRTimeSynchronizationClusterTimeZoneStruct
  ( MTRTimeSynchronizationClusterTimeZoneStruct
  , IsMTRTimeSynchronizationClusterTimeZoneStruct(..)
  , offset
  , setOffset
  , validAt
  , setValidAt
  , name
  , setName
  , offsetSelector
  , setOffsetSelector
  , validAtSelector
  , setValidAtSelector
  , nameSelector
  , setNameSelector


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

-- | @- offset@
offset :: IsMTRTimeSynchronizationClusterTimeZoneStruct mtrTimeSynchronizationClusterTimeZoneStruct => mtrTimeSynchronizationClusterTimeZoneStruct -> IO (Id NSNumber)
offset mtrTimeSynchronizationClusterTimeZoneStruct  =
    sendMsg mtrTimeSynchronizationClusterTimeZoneStruct (mkSelector "offset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOffset:@
setOffset :: (IsMTRTimeSynchronizationClusterTimeZoneStruct mtrTimeSynchronizationClusterTimeZoneStruct, IsNSNumber value) => mtrTimeSynchronizationClusterTimeZoneStruct -> value -> IO ()
setOffset mtrTimeSynchronizationClusterTimeZoneStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterTimeZoneStruct (mkSelector "setOffset:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- validAt@
validAt :: IsMTRTimeSynchronizationClusterTimeZoneStruct mtrTimeSynchronizationClusterTimeZoneStruct => mtrTimeSynchronizationClusterTimeZoneStruct -> IO (Id NSNumber)
validAt mtrTimeSynchronizationClusterTimeZoneStruct  =
    sendMsg mtrTimeSynchronizationClusterTimeZoneStruct (mkSelector "validAt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValidAt:@
setValidAt :: (IsMTRTimeSynchronizationClusterTimeZoneStruct mtrTimeSynchronizationClusterTimeZoneStruct, IsNSNumber value) => mtrTimeSynchronizationClusterTimeZoneStruct -> value -> IO ()
setValidAt mtrTimeSynchronizationClusterTimeZoneStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterTimeZoneStruct (mkSelector "setValidAt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRTimeSynchronizationClusterTimeZoneStruct mtrTimeSynchronizationClusterTimeZoneStruct => mtrTimeSynchronizationClusterTimeZoneStruct -> IO (Id NSString)
name mtrTimeSynchronizationClusterTimeZoneStruct  =
    sendMsg mtrTimeSynchronizationClusterTimeZoneStruct (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRTimeSynchronizationClusterTimeZoneStruct mtrTimeSynchronizationClusterTimeZoneStruct, IsNSString value) => mtrTimeSynchronizationClusterTimeZoneStruct -> value -> IO ()
setName mtrTimeSynchronizationClusterTimeZoneStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterTimeZoneStruct (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @offset@
offsetSelector :: Selector
offsetSelector = mkSelector "offset"

-- | @Selector@ for @setOffset:@
setOffsetSelector :: Selector
setOffsetSelector = mkSelector "setOffset:"

-- | @Selector@ for @validAt@
validAtSelector :: Selector
validAtSelector = mkSelector "validAt"

-- | @Selector@ for @setValidAt:@
setValidAtSelector :: Selector
setValidAtSelector = mkSelector "setValidAt:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

