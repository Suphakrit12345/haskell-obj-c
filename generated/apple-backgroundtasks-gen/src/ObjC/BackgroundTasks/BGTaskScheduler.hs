{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class for scheduling task requests that launch your app in the background.
--
-- Background tasks give your app a way to run code while the app is suspended. To learn how to register, schedule, and run a background task, see <doc://com.apple.documentation/documentation/uikit/app_and_environment/scenes/preparing_your_ui_to_run_in_the_background/using_background_tasks_to_update_your_app>.
--
-- Generated bindings for @BGTaskScheduler@.
module ObjC.BackgroundTasks.BGTaskScheduler
  ( BGTaskScheduler
  , IsBGTaskScheduler(..)
  , init_
  , new
  , registerForTaskWithIdentifier_usingQueue_launchHandler
  , submitTaskRequest_error
  , cancelTaskRequestWithIdentifier
  , cancelAllTaskRequests
  , sharedScheduler
  , supportedResources
  , initSelector
  , newSelector
  , registerForTaskWithIdentifier_usingQueue_launchHandlerSelector
  , submitTaskRequest_errorSelector
  , cancelTaskRequestWithIdentifierSelector
  , cancelAllTaskRequestsSelector
  , sharedSchedulerSelector
  , supportedResourcesSelector

  -- * Enum types
  , BGContinuedProcessingTaskRequestResources(BGContinuedProcessingTaskRequestResources)
  , pattern BGContinuedProcessingTaskRequestResourcesDefault
  , pattern BGContinuedProcessingTaskRequestResourcesGPU

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

-- | @- init@
init_ :: IsBGTaskScheduler bgTaskScheduler => bgTaskScheduler -> IO (Id BGTaskScheduler)
init_ bgTaskScheduler  =
    sendMsg bgTaskScheduler (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id BGTaskScheduler)
new  =
  do
    cls' <- getRequiredClass "BGTaskScheduler"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Register a launch handler for the task with the associated identifier that’s executed on the specified queue.
--
-- Every identifier in the <doc://com.apple.documentation/documentation/bundleresources/information_property_list/bgtaskschedulerpermittedidentifiers> requires a handler. Registration of all launch handlers must be complete before the end of <doc://com.apple.documentation/documentation/uikit/uiapplicationdelegate/1623053-applicationdidfinishlaunching>.
--
-- You must register launch handlers before your application finishes launching (``BGContinuedProcessingTask`` registrations are exempt from this requirement). Attempting to register a handler after launch or multiple handlers for the same identifier is an error. Although you may submit task requests from some extensions, only the host app will be launched to handle background work.
--
-- - Parameters:    - identifier: The identifier for the task that will be handled by the provided launch handler.    - queue: A queue for executing the task. Pass @nil@ to use a default background queue.    - launchHandler: The system runs the block of code for the launch handler when it launches the app in the background. The block takes a single parameter, a ``BGTask`` object used for assigning an expiration handler and for setting a completion status. The block has no return value. Assign an expiration handler to the task's expirationHandler property and call setTaskCompletedWithSuccess: when the background work is complete. - Returns: Returns <doc://com.apple.documentation/documentation/objectivec/yes> if the launch handler was registered. Returns <doc://com.apple.documentation/documentation/objectivec/no> if the identifier isn't included in the <doc://com.apple.documentation/documentation/bundleresources/information_property_list/bgtaskschedulerpermittedidentifiers> @Info.plist@. - Important: Register each task identifier only once. The system kills the app on the second registration of the same task identifier.
--
-- ObjC selector: @- registerForTaskWithIdentifier:usingQueue:launchHandler:@
registerForTaskWithIdentifier_usingQueue_launchHandler :: (IsBGTaskScheduler bgTaskScheduler, IsNSString identifier, IsNSObject queue) => bgTaskScheduler -> identifier -> queue -> Ptr () -> IO Bool
registerForTaskWithIdentifier_usingQueue_launchHandler bgTaskScheduler  identifier queue launchHandler =
  withObjCPtr identifier $ \raw_identifier ->
    withObjCPtr queue $ \raw_queue ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg bgTaskScheduler (mkSelector "registerForTaskWithIdentifier:usingQueue:launchHandler:") retCULong [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ()), argPtr (castPtr launchHandler :: Ptr ())]

-- | Submit a previously registered background task for execution.
--
-- Submitting a task request for an unexecuted task that’s already in the queue replaces the previous task request.
--
-- There can be a total of 1 refresh task and 10 processing tasks scheduled at any time. Trying to schedule more tasks returns ``BGTaskSchedulerErrorCode/BGTaskSchedulerErrorCodeTooManyPendingTaskRequests``.
--
-- - Parameters:   - taskRequest: The task request object representing the parameters of the background task to be scheduled.   - error: If an error occurs, upon return contains an error object that indicates why the request was rejected - Returns: @YES@ if the request was successfully submitted; @NO@ if there was an error
--
-- ObjC selector: @- submitTaskRequest:error:@
submitTaskRequest_error :: (IsBGTaskScheduler bgTaskScheduler, IsBGTaskRequest taskRequest, IsNSError error_) => bgTaskScheduler -> taskRequest -> error_ -> IO Bool
submitTaskRequest_error bgTaskScheduler  taskRequest error_ =
  withObjCPtr taskRequest $ \raw_taskRequest ->
    withObjCPtr error_ $ \raw_error_ ->
        fmap ((/= 0) :: CULong -> Bool) $ sendMsg bgTaskScheduler (mkSelector "submitTaskRequest:error:") retCULong [argPtr (castPtr raw_taskRequest :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Cancel a previously scheduled task request.
--
-- - Parameters:   - identifier: The identifier of the previously submitted task request to cancel.
--
-- ObjC selector: @- cancelTaskRequestWithIdentifier:@
cancelTaskRequestWithIdentifier :: (IsBGTaskScheduler bgTaskScheduler, IsNSString identifier) => bgTaskScheduler -> identifier -> IO ()
cancelTaskRequestWithIdentifier bgTaskScheduler  identifier =
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg bgTaskScheduler (mkSelector "cancelTaskRequestWithIdentifier:") retVoid [argPtr (castPtr raw_identifier :: Ptr ())]

-- | Cancel all previously submitted task requests.
--
-- ObjC selector: @- cancelAllTaskRequests@
cancelAllTaskRequests :: IsBGTaskScheduler bgTaskScheduler => bgTaskScheduler -> IO ()
cancelAllTaskRequests bgTaskScheduler  =
    sendMsg bgTaskScheduler (mkSelector "cancelAllTaskRequests") retVoid []

-- | The shared background task scheduler instance.
--
-- ObjC selector: @+ sharedScheduler@
sharedScheduler :: IO (Id BGTaskScheduler)
sharedScheduler  =
  do
    cls' <- getRequiredClass "BGTaskScheduler"
    sendClassMsg cls' (mkSelector "sharedScheduler") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A bitfield of the resources the device supports for ``BackgroundTasks/BGContinuedProcessingTaskRequest`` instances.
--
-- ObjC selector: @+ supportedResources@
supportedResources :: IO BGContinuedProcessingTaskRequestResources
supportedResources  =
  do
    cls' <- getRequiredClass "BGTaskScheduler"
    fmap (coerce :: CLong -> BGContinuedProcessingTaskRequestResources) $ sendClassMsg cls' (mkSelector "supportedResources") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @registerForTaskWithIdentifier:usingQueue:launchHandler:@
registerForTaskWithIdentifier_usingQueue_launchHandlerSelector :: Selector
registerForTaskWithIdentifier_usingQueue_launchHandlerSelector = mkSelector "registerForTaskWithIdentifier:usingQueue:launchHandler:"

-- | @Selector@ for @submitTaskRequest:error:@
submitTaskRequest_errorSelector :: Selector
submitTaskRequest_errorSelector = mkSelector "submitTaskRequest:error:"

-- | @Selector@ for @cancelTaskRequestWithIdentifier:@
cancelTaskRequestWithIdentifierSelector :: Selector
cancelTaskRequestWithIdentifierSelector = mkSelector "cancelTaskRequestWithIdentifier:"

-- | @Selector@ for @cancelAllTaskRequests@
cancelAllTaskRequestsSelector :: Selector
cancelAllTaskRequestsSelector = mkSelector "cancelAllTaskRequests"

-- | @Selector@ for @sharedScheduler@
sharedSchedulerSelector :: Selector
sharedSchedulerSelector = mkSelector "sharedScheduler"

-- | @Selector@ for @supportedResources@
supportedResourcesSelector :: Selector
supportedResourcesSelector = mkSelector "supportedResources"

