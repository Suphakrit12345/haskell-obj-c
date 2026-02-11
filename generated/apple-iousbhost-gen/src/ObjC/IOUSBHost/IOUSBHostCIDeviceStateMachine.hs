{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOUSBHostCIDeviceStateMachine
--
-- The object representing the state of a user-mode USB host controller device
--
-- This class assists with tracking internal state transitions of a user-mode USB host controller device, and parses IOUSBHostCIMessage command          structures to update state and generate properly formatted command responses.  Clients should create an IOUSBHostCIDeviceStateMachine in          response to an IOUSBHostCIMessageTypeDeviceCreate command, and then use the provided interfaces to identify and process commands          for the device.  The IOUSBHostCIDeviceStateMachine should be destroyed in response to an IOUSBHostCIMessageTypeDeviceDestroy command.
--
-- IOUSBHostCIDeviceStateMachine does not provide any concurrency protection, the client is responsible for necessary serialization.
--
-- Generated bindings for @IOUSBHostCIDeviceStateMachine@.
module ObjC.IOUSBHost.IOUSBHostCIDeviceStateMachine
  ( IOUSBHostCIDeviceStateMachine
  , IsIOUSBHostCIDeviceStateMachine(..)
  , init_
  , initWithInterface_command_error
  , inspectCommand_error
  , completeRoute
  , deviceAddress
  , controllerInterface
  , initSelector
  , initWithInterface_command_errorSelector
  , inspectCommand_errorSelector
  , completeRouteSelector
  , deviceAddressSelector
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
init_ :: IsIOUSBHostCIDeviceStateMachine iousbHostCIDeviceStateMachine => iousbHostCIDeviceStateMachine -> IO (Id IOUSBHostCIDeviceStateMachine)
init_ iousbHostCIDeviceStateMachine  =
    sendMsg iousbHostCIDeviceStateMachine (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Initializes an IOUSBHostCIDeviceStateMachine object
--
-- The IOUSBHostCIDeviceStateMachine defaults to the IOUSBHostCIDeviceStateActive state.
--
-- @interface@ — IOUSBHostControllerInterface which will be used to send command responses.
--
-- @command@ — IOUSBHostCIMessage with type IOUSBHostCIMessageTypeDeviceCreate
--
-- Returns: IOUSBHostCIDeviceStateMachine instance, to be released by the caller.
--
-- ObjC selector: @- initWithInterface:command:error:@
initWithInterface_command_error :: (IsIOUSBHostCIDeviceStateMachine iousbHostCIDeviceStateMachine, IsIOUSBHostControllerInterface interface, IsNSError error_) => iousbHostCIDeviceStateMachine -> interface -> Const (Ptr IOUSBHostCIMessage) -> error_ -> IO (Id IOUSBHostCIDeviceStateMachine)
initWithInterface_command_error iousbHostCIDeviceStateMachine  interface command error_ =
  withObjCPtr interface $ \raw_interface ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg iousbHostCIDeviceStateMachine (mkSelector "initWithInterface:command:error:") (retPtr retVoid) [argPtr (castPtr raw_interface :: Ptr ()), argPtr (unConst command), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | Inspect an IOUSBHostCIMessage command
--
-- The IOUSBHostCIMessage command is inspected to determine if it is handled by the state machine, and              is appropriate for the current state.
--
-- @command@ — IOUSBHostCIMessage command structure received from the kernel driver.
--
-- Returns: BOOL YES if the command is targeting a controller, and can be handled in the current state              BOOL NO if the command does not target a controller, or cannot be handled in the current state
--
-- ObjC selector: @- inspectCommand:error:@
inspectCommand_error :: (IsIOUSBHostCIDeviceStateMachine iousbHostCIDeviceStateMachine, IsNSError error_) => iousbHostCIDeviceStateMachine -> Const (Ptr IOUSBHostCIMessage) -> error_ -> IO Bool
inspectCommand_error iousbHostCIDeviceStateMachine  command error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostCIDeviceStateMachine (mkSelector "inspectCommand:error:") retCULong [argPtr (unConst command), argPtr (castPtr raw_error_ :: Ptr ())]

-- | @- completeRoute@
completeRoute :: IsIOUSBHostCIDeviceStateMachine iousbHostCIDeviceStateMachine => iousbHostCIDeviceStateMachine -> IO CULong
completeRoute iousbHostCIDeviceStateMachine  =
    sendMsg iousbHostCIDeviceStateMachine (mkSelector "completeRoute") retCULong []

-- | @- deviceAddress@
deviceAddress :: IsIOUSBHostCIDeviceStateMachine iousbHostCIDeviceStateMachine => iousbHostCIDeviceStateMachine -> IO CULong
deviceAddress iousbHostCIDeviceStateMachine  =
    sendMsg iousbHostCIDeviceStateMachine (mkSelector "deviceAddress") retCULong []

-- | @- controllerInterface@
controllerInterface :: IsIOUSBHostCIDeviceStateMachine iousbHostCIDeviceStateMachine => iousbHostCIDeviceStateMachine -> IO (Id IOUSBHostControllerInterface)
controllerInterface iousbHostCIDeviceStateMachine  =
    sendMsg iousbHostCIDeviceStateMachine (mkSelector "controllerInterface") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithInterface:command:error:@
initWithInterface_command_errorSelector :: Selector
initWithInterface_command_errorSelector = mkSelector "initWithInterface:command:error:"

-- | @Selector@ for @inspectCommand:error:@
inspectCommand_errorSelector :: Selector
inspectCommand_errorSelector = mkSelector "inspectCommand:error:"

-- | @Selector@ for @completeRoute@
completeRouteSelector :: Selector
completeRouteSelector = mkSelector "completeRoute"

-- | @Selector@ for @deviceAddress@
deviceAddressSelector :: Selector
deviceAddressSelector = mkSelector "deviceAddress"

-- | @Selector@ for @controllerInterface@
controllerInterfaceSelector :: Selector
controllerInterfaceSelector = mkSelector "controllerInterface"

