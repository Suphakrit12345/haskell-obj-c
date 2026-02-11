{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.BackgroundTasks.Internal.Classes (
    module ObjC.BackgroundTasks.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.Foundation.Internal.Classes

-- ---------- BGTask ----------

-- | An abstract class representing a task that’s run while the app is in the background.
-- 
-- Phantom type for @BGTask@.
data BGTask

instance IsObjCObject (Id BGTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BGTask"

class IsNSObject a => IsBGTask a where
  toBGTask :: a -> Id BGTask

instance IsBGTask (Id BGTask) where
  toBGTask = unsafeCastId

instance IsNSObject (Id BGTask) where
  toNSObject = unsafeCastId

-- ---------- BGTaskRequest ----------

-- | An abstract class that represents a request for the app to be launched in the background to perform work. Do not instantiate instances of this class directly. Instead, use one of its concrete subclasses.
-- 
-- Phantom type for @BGTaskRequest@.
data BGTaskRequest

instance IsObjCObject (Id BGTaskRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BGTaskRequest"

class IsNSObject a => IsBGTaskRequest a where
  toBGTaskRequest :: a -> Id BGTaskRequest

instance IsBGTaskRequest (Id BGTaskRequest) where
  toBGTaskRequest = unsafeCastId

instance IsNSObject (Id BGTaskRequest) where
  toNSObject = unsafeCastId

-- ---------- BGTaskScheduler ----------

-- | A class for scheduling task requests that launch your app in the background.
--
-- Background tasks give your app a way to run code while the app is suspended. To learn how to register, schedule, and run a background task, see <doc://com.apple.documentation/documentation/uikit/app_and_environment/scenes/preparing_your_ui_to_run_in_the_background/using_background_tasks_to_update_your_app>.
-- 
-- Phantom type for @BGTaskScheduler@.
data BGTaskScheduler

instance IsObjCObject (Id BGTaskScheduler) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BGTaskScheduler"

class IsNSObject a => IsBGTaskScheduler a where
  toBGTaskScheduler :: a -> Id BGTaskScheduler

instance IsBGTaskScheduler (Id BGTaskScheduler) where
  toBGTaskScheduler = unsafeCastId

instance IsNSObject (Id BGTaskScheduler) where
  toNSObject = unsafeCastId

-- ---------- BGAppRefreshTask ----------

-- | An object representing a short task typically used to refresh content that’s run while the app is in the background.
--
-- Use app refresh tasks for updating your app with small bits of information, such as the latest stock values.
--
-- Executing app refresh tasks requires setting the @fetch@ <doc://com.apple.documentation/documentation/bundleresources/information_property_list/uibackgroundmodes> capability. For information on setting this capability, see ``BGTaskScheduler``.
-- 
-- Phantom type for @BGAppRefreshTask@.
data BGAppRefreshTask

instance IsObjCObject (Id BGAppRefreshTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BGAppRefreshTask"

class IsBGTask a => IsBGAppRefreshTask a where
  toBGAppRefreshTask :: a -> Id BGAppRefreshTask

instance IsBGAppRefreshTask (Id BGAppRefreshTask) where
  toBGAppRefreshTask = unsafeCastId

instance IsBGTask (Id BGAppRefreshTask) where
  toBGTask = unsafeCastId

instance IsNSObject (Id BGAppRefreshTask) where
  toNSObject = unsafeCastId

-- ---------- BGContinuedProcessingTask ----------

-- | A task meant to perform processing on behalf of a user initiated request.
--
-- Continued processing tasks will present UI while in progress to provide awareness to the user. ``BGContinuedProcessingTask``s _must_ report progress via the ``NSProgressReporting`` protocol conformance during runtime and are subject to expiration based on changing system conditions and user input. Tasks that appear stalled may be forcibly expired by the scheduler to preserve system resources.
-- 
-- Phantom type for @BGContinuedProcessingTask@.
data BGContinuedProcessingTask

instance IsObjCObject (Id BGContinuedProcessingTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BGContinuedProcessingTask"

class IsBGTask a => IsBGContinuedProcessingTask a where
  toBGContinuedProcessingTask :: a -> Id BGContinuedProcessingTask

instance IsBGContinuedProcessingTask (Id BGContinuedProcessingTask) where
  toBGContinuedProcessingTask = unsafeCastId

instance IsBGTask (Id BGContinuedProcessingTask) where
  toBGTask = unsafeCastId

instance IsNSObject (Id BGContinuedProcessingTask) where
  toNSObject = unsafeCastId

-- ---------- BGProcessingTask ----------

-- | A time-consuming processing task that runs while the app is in the background.
--
-- Use processing tasks for long data updates, processing data, and app maintenance. Although processing tasks can run for minutes, the system can interrupt the process. Add an expiration handler by setting ``BGTask/expirationHandler`` for any required cleanup.
--
-- Executing processing tasks requires setting the @processing@ <doc://com.apple.documentation/documentation/bundleresources/information_property_list/uibackgroundmodes> capability. For information on setting this capability, see ``BGTaskScheduler``.
--
-- Processing tasks run only when the device is idle. The system terminates any background processing tasks running when the user starts using the device. Background refresh tasks are not affected.
-- 
-- Phantom type for @BGProcessingTask@.
data BGProcessingTask

instance IsObjCObject (Id BGProcessingTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BGProcessingTask"

class IsBGTask a => IsBGProcessingTask a where
  toBGProcessingTask :: a -> Id BGProcessingTask

instance IsBGProcessingTask (Id BGProcessingTask) where
  toBGProcessingTask = unsafeCastId

instance IsBGTask (Id BGProcessingTask) where
  toBGTask = unsafeCastId

instance IsNSObject (Id BGProcessingTask) where
  toNSObject = unsafeCastId

-- ---------- BGAppRefreshTaskRequest ----------

-- | A request to launch your app in the background to execute a short refresh task.
--
-- Schedule a refresh task request to ask that the system launch your app briefly so that you can download data and keep your app's contents up-to-date. The system will fulfill this request intelligently based on system conditions and app usage.
-- 
-- Phantom type for @BGAppRefreshTaskRequest@.
data BGAppRefreshTaskRequest

instance IsObjCObject (Id BGAppRefreshTaskRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BGAppRefreshTaskRequest"

class IsBGTaskRequest a => IsBGAppRefreshTaskRequest a where
  toBGAppRefreshTaskRequest :: a -> Id BGAppRefreshTaskRequest

instance IsBGAppRefreshTaskRequest (Id BGAppRefreshTaskRequest) where
  toBGAppRefreshTaskRequest = unsafeCastId

instance IsBGTaskRequest (Id BGAppRefreshTaskRequest) where
  toBGTaskRequest = unsafeCastId

instance IsNSObject (Id BGAppRefreshTaskRequest) where
  toNSObject = unsafeCastId

-- ---------- BGContinuedProcessingTaskRequest ----------

-- | A request to begin a workload immediately, or shortly after submission, which is allowed to continue running even if the app is backgrounded.
-- 
-- Phantom type for @BGContinuedProcessingTaskRequest@.
data BGContinuedProcessingTaskRequest

instance IsObjCObject (Id BGContinuedProcessingTaskRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BGContinuedProcessingTaskRequest"

class IsBGTaskRequest a => IsBGContinuedProcessingTaskRequest a where
  toBGContinuedProcessingTaskRequest :: a -> Id BGContinuedProcessingTaskRequest

instance IsBGContinuedProcessingTaskRequest (Id BGContinuedProcessingTaskRequest) where
  toBGContinuedProcessingTaskRequest = unsafeCastId

instance IsBGTaskRequest (Id BGContinuedProcessingTaskRequest) where
  toBGTaskRequest = unsafeCastId

instance IsNSObject (Id BGContinuedProcessingTaskRequest) where
  toNSObject = unsafeCastId

-- ---------- BGProcessingTaskRequest ----------

-- | A request to launch your app in the background to execute a processing task that can take minutes to complete.
--
-- Schedule a processing task request to ask that the system launch your app when conditions are favorable for battery life to handle deferrable, longer-running processing, such as syncing, database maintenance, or similar tasks. The system will attempt to fulfill this request to the best of its ability within the next two days as long as the user has used your app within the past week.
-- 
-- Phantom type for @BGProcessingTaskRequest@.
data BGProcessingTaskRequest

instance IsObjCObject (Id BGProcessingTaskRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BGProcessingTaskRequest"

class IsBGTaskRequest a => IsBGProcessingTaskRequest a where
  toBGProcessingTaskRequest :: a -> Id BGProcessingTaskRequest

instance IsBGProcessingTaskRequest (Id BGProcessingTaskRequest) where
  toBGProcessingTaskRequest = unsafeCastId

instance IsBGTaskRequest (Id BGProcessingTaskRequest) where
  toBGTaskRequest = unsafeCastId

instance IsNSObject (Id BGProcessingTaskRequest) where
  toNSObject = unsafeCastId

-- ---------- BGHealthResearchTask ----------

-- | A task meant to perform processing on behalf of health research studies.
--
-- Health research tasks may only be used by applications entitled to perform studies and user's have opted in to the relevant study. These apps must have the @com.apple.developer.backgroundtasks.healthresearch@ entitlement.
-- 
-- Phantom type for @BGHealthResearchTask@.
data BGHealthResearchTask

instance IsObjCObject (Id BGHealthResearchTask) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BGHealthResearchTask"

class IsBGProcessingTask a => IsBGHealthResearchTask a where
  toBGHealthResearchTask :: a -> Id BGHealthResearchTask

instance IsBGHealthResearchTask (Id BGHealthResearchTask) where
  toBGHealthResearchTask = unsafeCastId

instance IsBGProcessingTask (Id BGHealthResearchTask) where
  toBGProcessingTask = unsafeCastId

instance IsBGTask (Id BGHealthResearchTask) where
  toBGTask = unsafeCastId

instance IsNSObject (Id BGHealthResearchTask) where
  toNSObject = unsafeCastId

-- ---------- BGHealthResearchTaskRequest ----------

-- | A request to launch your app in the background to execute a health research task for studies a user has opted into and that can take minutes to complete.
-- 
-- Phantom type for @BGHealthResearchTaskRequest@.
data BGHealthResearchTaskRequest

instance IsObjCObject (Id BGHealthResearchTaskRequest) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "BGHealthResearchTaskRequest"

class IsBGProcessingTaskRequest a => IsBGHealthResearchTaskRequest a where
  toBGHealthResearchTaskRequest :: a -> Id BGHealthResearchTaskRequest

instance IsBGHealthResearchTaskRequest (Id BGHealthResearchTaskRequest) where
  toBGHealthResearchTaskRequest = unsafeCastId

instance IsBGProcessingTaskRequest (Id BGHealthResearchTaskRequest) where
  toBGProcessingTaskRequest = unsafeCastId

instance IsBGTaskRequest (Id BGHealthResearchTaskRequest) where
  toBGTaskRequest = unsafeCastId

instance IsNSObject (Id BGHealthResearchTaskRequest) where
  toNSObject = unsafeCastId
