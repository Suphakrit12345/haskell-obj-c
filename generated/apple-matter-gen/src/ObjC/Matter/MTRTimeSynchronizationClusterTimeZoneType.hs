{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterTimeZoneType@.
module ObjC.Matter.MTRTimeSynchronizationClusterTimeZoneType
  ( MTRTimeSynchronizationClusterTimeZoneType
  , IsMTRTimeSynchronizationClusterTimeZoneType(..)
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
offset :: IsMTRTimeSynchronizationClusterTimeZoneType mtrTimeSynchronizationClusterTimeZoneType => mtrTimeSynchronizationClusterTimeZoneType -> IO (Id NSNumber)
offset mtrTimeSynchronizationClusterTimeZoneType  =
    sendMsg mtrTimeSynchronizationClusterTimeZoneType (mkSelector "offset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOffset:@
setOffset :: (IsMTRTimeSynchronizationClusterTimeZoneType mtrTimeSynchronizationClusterTimeZoneType, IsNSNumber value) => mtrTimeSynchronizationClusterTimeZoneType -> value -> IO ()
setOffset mtrTimeSynchronizationClusterTimeZoneType  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterTimeZoneType (mkSelector "setOffset:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- validAt@
validAt :: IsMTRTimeSynchronizationClusterTimeZoneType mtrTimeSynchronizationClusterTimeZoneType => mtrTimeSynchronizationClusterTimeZoneType -> IO (Id NSNumber)
validAt mtrTimeSynchronizationClusterTimeZoneType  =
    sendMsg mtrTimeSynchronizationClusterTimeZoneType (mkSelector "validAt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValidAt:@
setValidAt :: (IsMTRTimeSynchronizationClusterTimeZoneType mtrTimeSynchronizationClusterTimeZoneType, IsNSNumber value) => mtrTimeSynchronizationClusterTimeZoneType -> value -> IO ()
setValidAt mtrTimeSynchronizationClusterTimeZoneType  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterTimeZoneType (mkSelector "setValidAt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- name@
name :: IsMTRTimeSynchronizationClusterTimeZoneType mtrTimeSynchronizationClusterTimeZoneType => mtrTimeSynchronizationClusterTimeZoneType -> IO (Id NSString)
name mtrTimeSynchronizationClusterTimeZoneType  =
    sendMsg mtrTimeSynchronizationClusterTimeZoneType (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setName:@
setName :: (IsMTRTimeSynchronizationClusterTimeZoneType mtrTimeSynchronizationClusterTimeZoneType, IsNSString value) => mtrTimeSynchronizationClusterTimeZoneType -> value -> IO ()
setName mtrTimeSynchronizationClusterTimeZoneType  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterTimeZoneType (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

