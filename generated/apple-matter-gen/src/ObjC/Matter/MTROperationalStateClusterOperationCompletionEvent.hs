{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROperationalStateClusterOperationCompletionEvent@.
module ObjC.Matter.MTROperationalStateClusterOperationCompletionEvent
  ( MTROperationalStateClusterOperationCompletionEvent
  , IsMTROperationalStateClusterOperationCompletionEvent(..)
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
completionErrorCode :: IsMTROperationalStateClusterOperationCompletionEvent mtrOperationalStateClusterOperationCompletionEvent => mtrOperationalStateClusterOperationCompletionEvent -> IO (Id NSNumber)
completionErrorCode mtrOperationalStateClusterOperationCompletionEvent  =
    sendMsg mtrOperationalStateClusterOperationCompletionEvent (mkSelector "completionErrorCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCompletionErrorCode:@
setCompletionErrorCode :: (IsMTROperationalStateClusterOperationCompletionEvent mtrOperationalStateClusterOperationCompletionEvent, IsNSNumber value) => mtrOperationalStateClusterOperationCompletionEvent -> value -> IO ()
setCompletionErrorCode mtrOperationalStateClusterOperationCompletionEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalStateClusterOperationCompletionEvent (mkSelector "setCompletionErrorCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- totalOperationalTime@
totalOperationalTime :: IsMTROperationalStateClusterOperationCompletionEvent mtrOperationalStateClusterOperationCompletionEvent => mtrOperationalStateClusterOperationCompletionEvent -> IO (Id NSNumber)
totalOperationalTime mtrOperationalStateClusterOperationCompletionEvent  =
    sendMsg mtrOperationalStateClusterOperationCompletionEvent (mkSelector "totalOperationalTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTotalOperationalTime:@
setTotalOperationalTime :: (IsMTROperationalStateClusterOperationCompletionEvent mtrOperationalStateClusterOperationCompletionEvent, IsNSNumber value) => mtrOperationalStateClusterOperationCompletionEvent -> value -> IO ()
setTotalOperationalTime mtrOperationalStateClusterOperationCompletionEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalStateClusterOperationCompletionEvent (mkSelector "setTotalOperationalTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pausedTime@
pausedTime :: IsMTROperationalStateClusterOperationCompletionEvent mtrOperationalStateClusterOperationCompletionEvent => mtrOperationalStateClusterOperationCompletionEvent -> IO (Id NSNumber)
pausedTime mtrOperationalStateClusterOperationCompletionEvent  =
    sendMsg mtrOperationalStateClusterOperationCompletionEvent (mkSelector "pausedTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPausedTime:@
setPausedTime :: (IsMTROperationalStateClusterOperationCompletionEvent mtrOperationalStateClusterOperationCompletionEvent, IsNSNumber value) => mtrOperationalStateClusterOperationCompletionEvent -> value -> IO ()
setPausedTime mtrOperationalStateClusterOperationCompletionEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrOperationalStateClusterOperationCompletionEvent (mkSelector "setPausedTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

