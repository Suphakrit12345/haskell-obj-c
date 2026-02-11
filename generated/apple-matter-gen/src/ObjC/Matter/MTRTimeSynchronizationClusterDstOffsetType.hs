{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterDstOffsetType@.
module ObjC.Matter.MTRTimeSynchronizationClusterDstOffsetType
  ( MTRTimeSynchronizationClusterDstOffsetType
  , IsMTRTimeSynchronizationClusterDstOffsetType(..)
  , offset
  , setOffset
  , validStarting
  , setValidStarting
  , validUntil
  , setValidUntil
  , offsetSelector
  , setOffsetSelector
  , validStartingSelector
  , setValidStartingSelector
  , validUntilSelector
  , setValidUntilSelector


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
offset :: IsMTRTimeSynchronizationClusterDstOffsetType mtrTimeSynchronizationClusterDstOffsetType => mtrTimeSynchronizationClusterDstOffsetType -> IO (Id NSNumber)
offset mtrTimeSynchronizationClusterDstOffsetType  =
    sendMsg mtrTimeSynchronizationClusterDstOffsetType (mkSelector "offset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOffset:@
setOffset :: (IsMTRTimeSynchronizationClusterDstOffsetType mtrTimeSynchronizationClusterDstOffsetType, IsNSNumber value) => mtrTimeSynchronizationClusterDstOffsetType -> value -> IO ()
setOffset mtrTimeSynchronizationClusterDstOffsetType  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterDstOffsetType (mkSelector "setOffset:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- validStarting@
validStarting :: IsMTRTimeSynchronizationClusterDstOffsetType mtrTimeSynchronizationClusterDstOffsetType => mtrTimeSynchronizationClusterDstOffsetType -> IO (Id NSNumber)
validStarting mtrTimeSynchronizationClusterDstOffsetType  =
    sendMsg mtrTimeSynchronizationClusterDstOffsetType (mkSelector "validStarting") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValidStarting:@
setValidStarting :: (IsMTRTimeSynchronizationClusterDstOffsetType mtrTimeSynchronizationClusterDstOffsetType, IsNSNumber value) => mtrTimeSynchronizationClusterDstOffsetType -> value -> IO ()
setValidStarting mtrTimeSynchronizationClusterDstOffsetType  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterDstOffsetType (mkSelector "setValidStarting:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- validUntil@
validUntil :: IsMTRTimeSynchronizationClusterDstOffsetType mtrTimeSynchronizationClusterDstOffsetType => mtrTimeSynchronizationClusterDstOffsetType -> IO (Id NSNumber)
validUntil mtrTimeSynchronizationClusterDstOffsetType  =
    sendMsg mtrTimeSynchronizationClusterDstOffsetType (mkSelector "validUntil") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValidUntil:@
setValidUntil :: (IsMTRTimeSynchronizationClusterDstOffsetType mtrTimeSynchronizationClusterDstOffsetType, IsNSNumber value) => mtrTimeSynchronizationClusterDstOffsetType -> value -> IO ()
setValidUntil mtrTimeSynchronizationClusterDstOffsetType  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterDstOffsetType (mkSelector "setValidUntil:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @offset@
offsetSelector :: Selector
offsetSelector = mkSelector "offset"

-- | @Selector@ for @setOffset:@
setOffsetSelector :: Selector
setOffsetSelector = mkSelector "setOffset:"

-- | @Selector@ for @validStarting@
validStartingSelector :: Selector
validStartingSelector = mkSelector "validStarting"

-- | @Selector@ for @setValidStarting:@
setValidStartingSelector :: Selector
setValidStartingSelector = mkSelector "setValidStarting:"

-- | @Selector@ for @validUntil@
validUntilSelector :: Selector
validUntilSelector = mkSelector "validUntil"

-- | @Selector@ for @setValidUntil:@
setValidUntilSelector :: Selector
setValidUntilSelector = mkSelector "setValidUntil:"

