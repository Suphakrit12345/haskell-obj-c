{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRRVCOperationalStateClusterOperationCompletionEvent@.
module ObjC.Matter.MTRRVCOperationalStateClusterOperationCompletionEvent
  ( MTRRVCOperationalStateClusterOperationCompletionEvent
  , IsMTRRVCOperationalStateClusterOperationCompletionEvent(..)
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
completionErrorCode :: IsMTRRVCOperationalStateClusterOperationCompletionEvent mtrrvcOperationalStateClusterOperationCompletionEvent => mtrrvcOperationalStateClusterOperationCompletionEvent -> IO (Id NSNumber)
completionErrorCode mtrrvcOperationalStateClusterOperationCompletionEvent  =
    sendMsg mtrrvcOperationalStateClusterOperationCompletionEvent (mkSelector "completionErrorCode") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCompletionErrorCode:@
setCompletionErrorCode :: (IsMTRRVCOperationalStateClusterOperationCompletionEvent mtrrvcOperationalStateClusterOperationCompletionEvent, IsNSNumber value) => mtrrvcOperationalStateClusterOperationCompletionEvent -> value -> IO ()
setCompletionErrorCode mtrrvcOperationalStateClusterOperationCompletionEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcOperationalStateClusterOperationCompletionEvent (mkSelector "setCompletionErrorCode:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- totalOperationalTime@
totalOperationalTime :: IsMTRRVCOperationalStateClusterOperationCompletionEvent mtrrvcOperationalStateClusterOperationCompletionEvent => mtrrvcOperationalStateClusterOperationCompletionEvent -> IO (Id NSNumber)
totalOperationalTime mtrrvcOperationalStateClusterOperationCompletionEvent  =
    sendMsg mtrrvcOperationalStateClusterOperationCompletionEvent (mkSelector "totalOperationalTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTotalOperationalTime:@
setTotalOperationalTime :: (IsMTRRVCOperationalStateClusterOperationCompletionEvent mtrrvcOperationalStateClusterOperationCompletionEvent, IsNSNumber value) => mtrrvcOperationalStateClusterOperationCompletionEvent -> value -> IO ()
setTotalOperationalTime mtrrvcOperationalStateClusterOperationCompletionEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcOperationalStateClusterOperationCompletionEvent (mkSelector "setTotalOperationalTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pausedTime@
pausedTime :: IsMTRRVCOperationalStateClusterOperationCompletionEvent mtrrvcOperationalStateClusterOperationCompletionEvent => mtrrvcOperationalStateClusterOperationCompletionEvent -> IO (Id NSNumber)
pausedTime mtrrvcOperationalStateClusterOperationCompletionEvent  =
    sendMsg mtrrvcOperationalStateClusterOperationCompletionEvent (mkSelector "pausedTime") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPausedTime:@
setPausedTime :: (IsMTRRVCOperationalStateClusterOperationCompletionEvent mtrrvcOperationalStateClusterOperationCompletionEvent, IsNSNumber value) => mtrrvcOperationalStateClusterOperationCompletionEvent -> value -> IO ()
setPausedTime mtrrvcOperationalStateClusterOperationCompletionEvent  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrrvcOperationalStateClusterOperationCompletionEvent (mkSelector "setPausedTime:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

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

