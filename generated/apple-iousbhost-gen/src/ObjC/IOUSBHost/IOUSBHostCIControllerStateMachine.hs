{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOUSBHostCIControllerStateMachine
--
-- The object representing the state of a user-mode USB host controller
--
-- This class assists with tracking internal state transitions of a user-mode USB host controller, and parses IOUSBHostCIMessage command          structures to update state and generate properly formatted command responses.
--
-- IOUSBHostCIControllerStateMachine does not provide any concurrency protection, the client is responsible for necessary serialization.
--
-- Generated bindings for @IOUSBHostCIControllerStateMachine@.
module ObjC.IOUSBHost.IOUSBHostCIControllerStateMachine
  ( IOUSBHostCIControllerStateMachine
  , IsIOUSBHostCIControllerStateMachine(..)
  , init_
  , initWithInterface_error
  , inspectCommand_error
  , enqueueUpdatedFrame_timestamp_error
  , controllerInterface
  , initSelector
  , initWithInterface_errorSelector
  , inspectCommand_errorSelector
  , enqueueUpdatedFrame_timestamp_errorSelector
  , controllerInterfaceSelector


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

import ObjC.IOUSBHost.Internal.Classes
import ObjC.IOUSBHost.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsIOUSBHostCIControllerStateMachine iousbHostCIControllerStateMachine => iousbHostCIControllerStateMachine -> IO (Id IOUSBHostCIControllerStateMachine)
init_ iousbHostCIControllerStateMachine  =
    sendMsg iousbHostCIControllerStateMachine (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initializes an IOUSBHostCIControllerStateMachine object
--
-- The IOUSBHostCIControllerStateMachine defaults to the IOUSBHostCIControllerStateOff state.
--
-- @interface@ — IOUSBHostControllerInterface which will be used to send command responses.
--
-- Returns: IOUSBHostCIControllerStateMachine instance, to be released by the caller.
--
-- ObjC selector: @- initWithInterface:error:@
initWithInterface_error :: (IsIOUSBHostCIControllerStateMachine iousbHostCIControllerStateMachine, IsIOUSBHostControllerInterface interface, IsNSError error_) => iousbHostCIControllerStateMachine -> interface -> error_ -> IO (Id IOUSBHostCIControllerStateMachine)
initWithInterface_error iousbHostCIControllerStateMachine  interface error_ =
  withObjCPtr interface $ \raw_interface ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg iousbHostCIControllerStateMachine (mkSelector "initWithInterface:error:") (retPtr retVoid) [argPtr (castPtr raw_interface :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Inspect an IOUSBHostCIMessage command
--
-- The IOUSBHostCIMessage command is inspected to determine if it is handled by the state machine, and              is appropriate for the current state.
--
-- @command@ — IOUSBHostCIMessage command structure received from the kernel driver.
--
-- Returns: BOOL YES if the command is targeting a controller, and can be handled in the current state              BOOL NO if the command does not target a controller, or cannot be handled in the current state
--
-- ObjC selector: @- inspectCommand:error:@
inspectCommand_error :: (IsIOUSBHostCIControllerStateMachine iousbHostCIControllerStateMachine, IsNSError error_) => iousbHostCIControllerStateMachine -> Const (Ptr IOUSBHostCIMessage) -> error_ -> IO Bool
inspectCommand_error iousbHostCIControllerStateMachine  command error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostCIControllerStateMachine (mkSelector "inspectCommand:error:") retCULong [argPtr (unConst command), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Enqueue frame and timestamp messages for delivery to the kernel driver
--
-- If the controller interface is in the IOUSBHostCIControllerStateActive state, messages with the type IOUSBHostCIMessageTypeFrameNumberUpdate and              IOUSBHostCIMessageTypeFrameTimestampUpdate will be generated using the provided inputs, and enqueued for delivery to the kernel driver.              The frame and timestamp information provided effectively measure the duration of the controller's 1ms frame in terms of system time.  A 1% frame duration              variation is permitted.  A larger frame duration variation will result in a IOUSBHostCIExceptionTypeFrameUpdateError.
--
-- @frame@ — uint64_t containing the number of 1ms frames that have elapsed since the controller began counting frames
--
-- @timestamp@ — uint64_t containing the mach_absolute_time() correlated to the beginning of the frameNumber
--
-- Returns: BOOL YES if the messages were enqueued for delivery to the kernel.              BOOL NO if the messages were not enqueued for delivery to the kernel.
--
-- ObjC selector: @- enqueueUpdatedFrame:timestamp:error:@
enqueueUpdatedFrame_timestamp_error :: (IsIOUSBHostCIControllerStateMachine iousbHostCIControllerStateMachine, IsNSError error_) => iousbHostCIControllerStateMachine -> CULong -> CULong -> error_ -> IO Bool
enqueueUpdatedFrame_timestamp_error iousbHostCIControllerStateMachine  frame timestamp error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostCIControllerStateMachine (mkSelector "enqueueUpdatedFrame:timestamp:error:") retCULong [argCULong frame, argCULong timestamp, argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- controllerInterface@
controllerInterface :: IsIOUSBHostCIControllerStateMachine iousbHostCIControllerStateMachine => iousbHostCIControllerStateMachine -> IO (Id IOUSBHostControllerInterface)
controllerInterface iousbHostCIControllerStateMachine  =
    sendMsg iousbHostCIControllerStateMachine (mkSelector "controllerInterface") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithInterface:error:@
initWithInterface_errorSelector :: Selector
initWithInterface_errorSelector = mkSelector "initWithInterface:error:"

-- | @Selector@ for @inspectCommand:error:@
inspectCommand_errorSelector :: Selector
inspectCommand_errorSelector = mkSelector "inspectCommand:error:"

-- | @Selector@ for @enqueueUpdatedFrame:timestamp:error:@
enqueueUpdatedFrame_timestamp_errorSelector :: Selector
enqueueUpdatedFrame_timestamp_errorSelector = mkSelector "enqueueUpdatedFrame:timestamp:error:"

-- | @Selector@ for @controllerInterface@
controllerInterfaceSelector :: Selector
controllerInterfaceSelector = mkSelector "controllerInterface"

