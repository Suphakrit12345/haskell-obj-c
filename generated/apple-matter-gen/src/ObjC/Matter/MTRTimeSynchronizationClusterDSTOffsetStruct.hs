{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRTimeSynchronizationClusterDSTOffsetStruct@.
module ObjC.Matter.MTRTimeSynchronizationClusterDSTOffsetStruct
  ( MTRTimeSynchronizationClusterDSTOffsetStruct
  , IsMTRTimeSynchronizationClusterDSTOffsetStruct(..)
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
offset :: IsMTRTimeSynchronizationClusterDSTOffsetStruct mtrTimeSynchronizationClusterDSTOffsetStruct => mtrTimeSynchronizationClusterDSTOffsetStruct -> IO (Id NSNumber)
offset mtrTimeSynchronizationClusterDSTOffsetStruct  =
    sendMsg mtrTimeSynchronizationClusterDSTOffsetStruct (mkSelector "offset") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setOffset:@
setOffset :: (IsMTRTimeSynchronizationClusterDSTOffsetStruct mtrTimeSynchronizationClusterDSTOffsetStruct, IsNSNumber value) => mtrTimeSynchronizationClusterDSTOffsetStruct -> value -> IO ()
setOffset mtrTimeSynchronizationClusterDSTOffsetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterDSTOffsetStruct (mkSelector "setOffset:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- validStarting@
validStarting :: IsMTRTimeSynchronizationClusterDSTOffsetStruct mtrTimeSynchronizationClusterDSTOffsetStruct => mtrTimeSynchronizationClusterDSTOffsetStruct -> IO (Id NSNumber)
validStarting mtrTimeSynchronizationClusterDSTOffsetStruct  =
    sendMsg mtrTimeSynchronizationClusterDSTOffsetStruct (mkSelector "validStarting") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValidStarting:@
setValidStarting :: (IsMTRTimeSynchronizationClusterDSTOffsetStruct mtrTimeSynchronizationClusterDSTOffsetStruct, IsNSNumber value) => mtrTimeSynchronizationClusterDSTOffsetStruct -> value -> IO ()
setValidStarting mtrTimeSynchronizationClusterDSTOffsetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterDSTOffsetStruct (mkSelector "setValidStarting:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- validUntil@
validUntil :: IsMTRTimeSynchronizationClusterDSTOffsetStruct mtrTimeSynchronizationClusterDSTOffsetStruct => mtrTimeSynchronizationClusterDSTOffsetStruct -> IO (Id NSNumber)
validUntil mtrTimeSynchronizationClusterDSTOffsetStruct  =
    sendMsg mtrTimeSynchronizationClusterDSTOffsetStruct (mkSelector "validUntil") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setValidUntil:@
setValidUntil :: (IsMTRTimeSynchronizationClusterDSTOffsetStruct mtrTimeSynchronizationClusterDSTOffsetStruct, IsNSNumber value) => mtrTimeSynchronizationClusterDSTOffsetStruct -> value -> IO ()
setValidUntil mtrTimeSynchronizationClusterDSTOffsetStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrTimeSynchronizationClusterDSTOffsetStruct (mkSelector "setValidUntil:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

