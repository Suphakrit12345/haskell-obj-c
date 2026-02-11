{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request to begin a workload immediately, or shortly after submission, which is allowed to continue running even if the app is backgrounded.
--
-- Generated bindings for @BGContinuedProcessingTaskRequest@.
module ObjC.BackgroundTasks.BGContinuedProcessingTaskRequest
  ( BGContinuedProcessingTaskRequest
  , IsBGContinuedProcessingTaskRequest(..)
  , initWithIdentifier_title_subtitle
  , title
  , setTitle
  , subtitle
  , setSubtitle
  , strategy
  , setStrategy
  , requiredResources
  , setRequiredResources
  , initWithIdentifier_title_subtitleSelector
  , titleSelector
  , setTitleSelector
  , subtitleSelector
  , setSubtitleSelector
  , strategySelector
  , setStrategySelector
  , requiredResourcesSelector
  , setRequiredResourcesSelector

  -- * Enum types
  , BGContinuedProcessingTaskRequestResources(BGContinuedProcessingTaskRequestResources)
  , pattern BGContinuedProcessingTaskRequestResourcesDefault
  , pattern BGContinuedProcessingTaskRequestResourcesGPU
  , BGContinuedProcessingTaskRequestSubmissionStrategy(BGContinuedProcessingTaskRequestSubmissionStrategy)
  , pattern BGContinuedProcessingTaskRequestSubmissionStrategyFail
  , pattern BGContinuedProcessingTaskRequestSubmissionStrategyQueue

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

import ObjC.BackgroundTasks.Internal.Classes
import ObjC.BackgroundTasks.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | Creates an instance on behalf of the currently foregrounded app.
--
-- Apps and their extensions should use this method to initialize any tasks due to the underlying association to the currently foregrounded app. Please note that ``BGTaskRequest/earliestBeginDate`` will be outright ignored by the scheduler in favor of @NSDate.now@.
--
-- The identifier ought to use wildcard notation, where the prefix of the identifier must at least contain the bundle ID of the submitting application, followed by optional semantic context, and finally ending with @.*@. An example: `<MainBundle>.<SemanticContext>.*@ which would transform to @com.foo.MyApplication.continuedProcessingTask.*`. Thus, a submitted identifier would be of the form @com.foo.MyApplication.continuedProcessingTask.HD830D@.
--
-- - Parameters:   - identifier: The task identifier.   - title: The localized title displayed to the user before the task begins running.   - subtitle: The localized subtitle displayed to the user before the task begins running. - Warning: Successful creation of this object does not guarantee successful submission to the scheduler.
--
-- ObjC selector: @- initWithIdentifier:title:subtitle:@
initWithIdentifier_title_subtitle :: (IsBGContinuedProcessingTaskRequest bgContinuedProcessingTaskRequest, IsNSString identifier, IsNSString title, IsNSString subtitle) => bgContinuedProcessingTaskRequest -> identifier -> title -> subtitle -> IO (Id BGContinuedProcessingTaskRequest)
initWithIdentifier_title_subtitle bgContinuedProcessingTaskRequest  identifier title subtitle =
  withObjCPtr identifier $ \raw_identifier ->
    withObjCPtr title $ \raw_title ->
      withObjCPtr subtitle $ \raw_subtitle ->
          sendMsg bgContinuedProcessingTaskRequest (mkSelector "initWithIdentifier:title:subtitle:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_title :: Ptr ()), argPtr (castPtr raw_subtitle :: Ptr ())] >>= ownedObject . castPtr

-- | The localized title displayed to the user.
--
-- ObjC selector: @- title@
title :: IsBGContinuedProcessingTaskRequest bgContinuedProcessingTaskRequest => bgContinuedProcessingTaskRequest -> IO (Id NSString)
title bgContinuedProcessingTaskRequest  =
    sendMsg bgContinuedProcessingTaskRequest (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The localized title displayed to the user.
--
-- ObjC selector: @- setTitle:@
setTitle :: (IsBGContinuedProcessingTaskRequest bgContinuedProcessingTaskRequest, IsNSString value) => bgContinuedProcessingTaskRequest -> value -> IO ()
setTitle bgContinuedProcessingTaskRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg bgContinuedProcessingTaskRequest (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The localized subtitle displayed to the user.
--
-- ObjC selector: @- subtitle@
subtitle :: IsBGContinuedProcessingTaskRequest bgContinuedProcessingTaskRequest => bgContinuedProcessingTaskRequest -> IO (Id NSString)
subtitle bgContinuedProcessingTaskRequest  =
    sendMsg bgContinuedProcessingTaskRequest (mkSelector "subtitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The localized subtitle displayed to the user.
--
-- ObjC selector: @- setSubtitle:@
setSubtitle :: (IsBGContinuedProcessingTaskRequest bgContinuedProcessingTaskRequest, IsNSString value) => bgContinuedProcessingTaskRequest -> value -> IO ()
setSubtitle bgContinuedProcessingTaskRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg bgContinuedProcessingTaskRequest (mkSelector "setSubtitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | The submission strategy for the scheduler to abide by.
--
-- Defaults to ``BGContinuedProcessingTaskRequestSubmissionStrategy/BGContinuedProcessingTaskRequestSubmissionStrategyQueue``.
--
-- ObjC selector: @- strategy@
strategy :: IsBGContinuedProcessingTaskRequest bgContinuedProcessingTaskRequest => bgContinuedProcessingTaskRequest -> IO BGContinuedProcessingTaskRequestSubmissionStrategy
strategy bgContinuedProcessingTaskRequest  =
    fmap (coerce :: CLong -> BGContinuedProcessingTaskRequestSubmissionStrategy) $ sendMsg bgContinuedProcessingTaskRequest (mkSelector "strategy") retCLong []

-- | The submission strategy for the scheduler to abide by.
--
-- Defaults to ``BGContinuedProcessingTaskRequestSubmissionStrategy/BGContinuedProcessingTaskRequestSubmissionStrategyQueue``.
--
-- ObjC selector: @- setStrategy:@
setStrategy :: IsBGContinuedProcessingTaskRequest bgContinuedProcessingTaskRequest => bgContinuedProcessingTaskRequest -> BGContinuedProcessingTaskRequestSubmissionStrategy -> IO ()
setStrategy bgContinuedProcessingTaskRequest  value =
    sendMsg bgContinuedProcessingTaskRequest (mkSelector "setStrategy:") retVoid [argCLong (coerce value)]

-- | Inform the scheduler that the task will be requesting additional system resources.
--
-- Defaults to ``BGContinuedProcessingTaskRequestResources/BGContinuedProcessingTaskRequestResourcesDefault``.
--
-- ObjC selector: @- requiredResources@
requiredResources :: IsBGContinuedProcessingTaskRequest bgContinuedProcessingTaskRequest => bgContinuedProcessingTaskRequest -> IO BGContinuedProcessingTaskRequestResources
requiredResources bgContinuedProcessingTaskRequest  =
    fmap (coerce :: CLong -> BGContinuedProcessingTaskRequestResources) $ sendMsg bgContinuedProcessingTaskRequest (mkSelector "requiredResources") retCLong []

-- | Inform the scheduler that the task will be requesting additional system resources.
--
-- Defaults to ``BGContinuedProcessingTaskRequestResources/BGContinuedProcessingTaskRequestResourcesDefault``.
--
-- ObjC selector: @- setRequiredResources:@
setRequiredResources :: IsBGContinuedProcessingTaskRequest bgContinuedProcessingTaskRequest => bgContinuedProcessingTaskRequest -> BGContinuedProcessingTaskRequestResources -> IO ()
setRequiredResources bgContinuedProcessingTaskRequest  value =
    sendMsg bgContinuedProcessingTaskRequest (mkSelector "setRequiredResources:") retVoid [argCLong (coerce value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:title:subtitle:@
initWithIdentifier_title_subtitleSelector :: Selector
initWithIdentifier_title_subtitleSelector = mkSelector "initWithIdentifier:title:subtitle:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector
setSubtitleSelector = mkSelector "setSubtitle:"

-- | @Selector@ for @strategy@
strategySelector :: Selector
strategySelector = mkSelector "strategy"

-- | @Selector@ for @setStrategy:@
setStrategySelector :: Selector
setStrategySelector = mkSelector "setStrategy:"

-- | @Selector@ for @requiredResources@
requiredResourcesSelector :: Selector
requiredResourcesSelector = mkSelector "requiredResources"

-- | @Selector@ for @setRequiredResources:@
setRequiredResourcesSelector :: Selector
setRequiredResourcesSelector = mkSelector "setRequiredResources:"

