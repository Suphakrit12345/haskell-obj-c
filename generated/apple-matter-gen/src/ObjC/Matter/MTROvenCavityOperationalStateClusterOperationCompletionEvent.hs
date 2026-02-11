{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROvenCavityOperationalStateClusterOperationCompletionEvent@.
module ObjC.Matter.MTROvenCavityOperationalStateClusterOperationCompletionEvent
  ( MTROvenCavityOperationalStateClusterOperationCompletionEvent
  , IsMTROvenCavityOperationalStateClusterOperationCompletionEvent(..)
  , completionErrorCode
  , setCompletionErrorCode
  , totalOperationalTime
  , setTotalOperationalTime
  , pausedTime
  , setPausedTime
  , completionErrorCodeSelector
  , setCompletionErrorCodeSelector
  , totalOperationalTimeSelector
  , setTotalOperationalTimeSelector
  , pausedTimeSelector
  , setPausedTimeSelector


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

-- | @- completionErrorCode@
completionErrorCode :: IsMTROvenCavityOperationalStateClusterOperationCompletionEvent mtrOvenCavityOperationalStateClusterOperationCompletionEvent => mtrOvenCavityOperationalStateClusterOperationCompletionEvent -> IO (Id NSNumber)
completionErrorCode mtrOvenCavityOperationalStateClusterOperationCompletionEvent  =
    sendMsg mtrOvenCavityOperationalStateClusterOperationCompletionEvent (mkSelector "completionErrorCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCompletionErrorCode:@
setCompletionErrorCode :: (IsMTROvenCavityOperationalStateClusterOperationCompletionEvent mtrOvenCavityOperationalStateClusterOperationCompletionEvent, IsNSNumber value) => mtrOvenCavityOperationalStateClusterOperationCompletionEvent -> value -> IO ()
setCompletionErrorCode mtrOvenCavityOperationalStateClusterOperationCompletionEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOvenCavityOperationalStateClusterOperationCompletionEvent (mkSelector "setCompletionErrorCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- totalOperationalTime@
totalOperationalTime :: IsMTROvenCavityOperationalStateClusterOperationCompletionEvent mtrOvenCavityOperationalStateClusterOperationCompletionEvent => mtrOvenCavityOperationalStateClusterOperationCompletionEvent -> IO (Id NSNumber)
totalOperationalTime mtrOvenCavityOperationalStateClusterOperationCompletionEvent  =
    sendMsg mtrOvenCavityOperationalStateClusterOperationCompletionEvent (mkSelector "totalOperationalTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTotalOperationalTime:@
setTotalOperationalTime :: (IsMTROvenCavityOperationalStateClusterOperationCompletionEvent mtrOvenCavityOperationalStateClusterOperationCompletionEvent, IsNSNumber value) => mtrOvenCavityOperationalStateClusterOperationCompletionEvent -> value -> IO ()
setTotalOperationalTime mtrOvenCavityOperationalStateClusterOperationCompletionEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOvenCavityOperationalStateClusterOperationCompletionEvent (mkSelector "setTotalOperationalTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pausedTime@
pausedTime :: IsMTROvenCavityOperationalStateClusterOperationCompletionEvent mtrOvenCavityOperationalStateClusterOperationCompletionEvent => mtrOvenCavityOperationalStateClusterOperationCompletionEvent -> IO (Id NSNumber)
pausedTime mtrOvenCavityOperationalStateClusterOperationCompletionEvent  =
    sendMsg mtrOvenCavityOperationalStateClusterOperationCompletionEvent (mkSelector "pausedTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPausedTime:@
setPausedTime :: (IsMTROvenCavityOperationalStateClusterOperationCompletionEvent mtrOvenCavityOperationalStateClusterOperationCompletionEvent, IsNSNumber value) => mtrOvenCavityOperationalStateClusterOperationCompletionEvent -> value -> IO ()
setPausedTime mtrOvenCavityOperationalStateClusterOperationCompletionEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOvenCavityOperationalStateClusterOperationCompletionEvent (mkSelector "setPausedTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @completionErrorCode@
completionErrorCodeSelector :: Selector
completionErrorCodeSelector = mkSelector "completionErrorCode"

-- | @Selector@ for @setCompletionErrorCode:@
setCompletionErrorCodeSelector :: Selector
setCompletionErrorCodeSelector = mkSelector "setCompletionErrorCode:"

-- | @Selector@ for @totalOperationalTime@
totalOperationalTimeSelector :: Selector
totalOperationalTimeSelector = mkSelector "totalOperationalTime"

-- | @Selector@ for @setTotalOperationalTime:@
setTotalOperationalTimeSelector :: Selector
setTotalOperationalTimeSelector = mkSelector "setTotalOperationalTime:"

-- | @Selector@ for @pausedTime@
pausedTimeSelector :: Selector
pausedTimeSelector = mkSelector "pausedTime"

-- | @Selector@ for @setPausedTime:@
setPausedTimeSelector :: Selector
setPausedTimeSelector = mkSelector "setPausedTime:"

