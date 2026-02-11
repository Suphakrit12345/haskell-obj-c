{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | IOUSBHostDevice
--
-- The IOUSBHostObject representing a USB device
--
-- This class provides functionality to send control requests to the default control endpoint
--
-- Generated bindings for @IOUSBHostDevice@.
module ObjC.IOUSBHost.IOUSBHostDevice
  ( IOUSBHostDevice
  , IsIOUSBHostDevice(..)
  , createMatchingDictionaryWithVendorID_productID_bcdDevice_deviceClass_deviceSubclass_deviceProtocol_speed_productIDArray
  , configureWithValue_matchInterfaces_error
  , configureWithValue_error
  , resetWithError
  , configurationDescriptor
  , createMatchingDictionaryWithVendorID_productID_bcdDevice_deviceClass_deviceSubclass_deviceProtocol_speed_productIDArraySelector
  , configureWithValue_matchInterfaces_errorSelector
  , configureWithValue_errorSelector
  , resetWithErrorSelector
  , configurationDescriptorSelector


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
import ObjC.IOKit.Internal.Structs
import ObjC.Foundation.Internal.Classes

-- | Creates a matching dictionary for an IOUSBHostDevice to be passed into              IOServiceGetMatchingService
--
-- @vendorID@ — NSNumber representation of device vendorID
--
-- @productID@ — NSNumber representation of device productID
--
-- @bcdDevice@ — NSNumber representation of device release number
--
-- @deviceClass@ — NSNumber representation of device class
--
-- @deviceSubclass@ — NSNumber representation of device subclass
--
-- @deviceProtocol@ — NSNumber representation of device protocol
--
-- @speed@ — NSNumber representation of device speed
--
-- @productIDArray@ — NSArray of NSNumbers representing all productIDs interested in.              If used do not specify the NSNumber productID field
--
-- Returns: CFMutableDictionaryRef to be used with IOService matching methods. To be released by              caller.
--
-- ObjC selector: @+ createMatchingDictionaryWithVendorID:productID:bcdDevice:deviceClass:deviceSubclass:deviceProtocol:speed:productIDArray:@
createMatchingDictionaryWithVendorID_productID_bcdDevice_deviceClass_deviceSubclass_deviceProtocol_speed_productIDArray :: (IsNSNumber vendorID, IsNSNumber productID, IsNSNumber bcdDevice, IsNSNumber deviceClass, IsNSNumber deviceSubclass, IsNSNumber deviceProtocol, IsNSNumber speed, IsNSArray productIDArray) => vendorID -> productID -> bcdDevice -> deviceClass -> deviceSubclass -> deviceProtocol -> speed -> productIDArray -> IO (Ptr ())
createMatchingDictionaryWithVendorID_productID_bcdDevice_deviceClass_deviceSubclass_deviceProtocol_speed_productIDArray vendorID productID bcdDevice deviceClass deviceSubclass deviceProtocol speed productIDArray =
  do
    cls' <- getRequiredClass "IOUSBHostDevice"
    withObjCPtr vendorID $ \raw_vendorID ->
      withObjCPtr productID $ \raw_productID ->
        withObjCPtr bcdDevice $ \raw_bcdDevice ->
          withObjCPtr deviceClass $ \raw_deviceClass ->
            withObjCPtr deviceSubclass $ \raw_deviceSubclass ->
              withObjCPtr deviceProtocol $ \raw_deviceProtocol ->
                withObjCPtr speed $ \raw_speed ->
                  withObjCPtr productIDArray $ \raw_productIDArray ->
                    fmap castPtr $ sendClassMsg cls' (mkSelector "createMatchingDictionaryWithVendorID:productID:bcdDevice:deviceClass:deviceSubclass:deviceProtocol:speed:productIDArray:") (retPtr retVoid) [argPtr (castPtr raw_vendorID :: Ptr ()), argPtr (castPtr raw_productID :: Ptr ()), argPtr (castPtr raw_bcdDevice :: Ptr ()), argPtr (castPtr raw_deviceClass :: Ptr ()), argPtr (castPtr raw_deviceSubclass :: Ptr ()), argPtr (castPtr raw_deviceProtocol :: Ptr ()), argPtr (castPtr raw_speed :: Ptr ()), argPtr (castPtr raw_productIDArray :: Ptr ())]

-- | Select a new configuration for the device
--
-- This method will select a new configuration for a device. If the device was              previously configured all child interfaces will be terminated prior to setting              the new configuration.  This method will send the SET_CONFIGURATION control request              (USB 2.0 9.4.7) to the device. The interfaces will be registered for matching by              default. After the completion of this call, the interfaces are not guaranteed              to be immediately available.
--
-- @value@ — Configuration value to select
--
-- @matchInterfaces@ — If YES, any interfaces within the new configuration will be              registered for matching. By default this is set to YES.
--
-- Returns: YES on success, an IOReturn error code will be reported on failure.
--
-- ObjC selector: @- configureWithValue:matchInterfaces:error:@
configureWithValue_matchInterfaces_error :: (IsIOUSBHostDevice iousbHostDevice, IsNSError error_) => iousbHostDevice -> CULong -> Bool -> error_ -> IO Bool
configureWithValue_matchInterfaces_error iousbHostDevice  value matchInterfaces error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostDevice (mkSelector "configureWithValue:matchInterfaces:error:") retCULong [argCULong value, argCULong (if matchInterfaces then 1 else 0), argPtr (castPtr raw_error_ :: Ptr ())]

-- | Select a new configuration for the device
--
-- This method will select a new configuration for a device.  If the device was              previously configured all child interfaces will be terminated prior to setting              the new configuration.  This method will send the SET_CONFIGURATION control request              (USB 2.0 9.4.7) to the device. The interfaces will be registered for matching by              default. After the completion of this call, the interfaces are not guaranteed              to be immediately available.
--
-- @value@ — Configuration value to select
--
-- Returns: YES on success, an IOReturn error code will be reported on failure.
--
-- ObjC selector: @- configureWithValue:error:@
configureWithValue_error :: (IsIOUSBHostDevice iousbHostDevice, IsNSError error_) => iousbHostDevice -> CULong -> error_ -> IO Bool
configureWithValue_error iousbHostDevice  value error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostDevice (mkSelector "configureWithValue:error:") retCULong [argCULong value, argPtr (castPtr raw_error_ :: Ptr ())]

-- | Terminate the device and attempt to reenumerate it
--
-- This function will reset and attempt to reenumerate the USB device.              The current IOUSBHostDevice object and all of its children will be terminated.              A new IOUSBHostDevice IOService object will be created and registered if the reset              is successful and the previous object has finished terminating. The framework IOUSBHostDevice              will no longer have a valid connection with IOService userclient after the call returns              successfully. A new framework client will need to be created to use the re-enumerated device.
--
-- Returns: YES on success, an IOReturn error code will be reported on failure.
--
-- ObjC selector: @- resetWithError:@
resetWithError :: (IsIOUSBHostDevice iousbHostDevice, IsNSError error_) => iousbHostDevice -> error_ -> IO Bool
resetWithError iousbHostDevice  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      fmap ((/= 0) :: CULong -> Bool) $ sendMsg iousbHostDevice (mkSelector "resetWithError:") retCULong [argPtr (castPtr raw_error_ :: Ptr ())]

-- | Return the currently selected configuration descriptor
--
-- This method uses descriptorWithType to return the configuration descriptor currently              selected after a successful setConfiguration call
--
-- Returns: Pointer to the configuration descriptor if found, or nil if the device is not              configured
--
-- ObjC selector: @- configurationDescriptor@
configurationDescriptor :: IsIOUSBHostDevice iousbHostDevice => iousbHostDevice -> IO (Const (Ptr IOUSBConfigurationDescriptor))
configurationDescriptor iousbHostDevice  =
    fmap Const $ fmap castPtr $ sendMsg iousbHostDevice (mkSelector "configurationDescriptor") (retPtr retVoid) []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @createMatchingDictionaryWithVendorID:productID:bcdDevice:deviceClass:deviceSubclass:deviceProtocol:speed:productIDArray:@
createMatchingDictionaryWithVendorID_productID_bcdDevice_deviceClass_deviceSubclass_deviceProtocol_speed_productIDArraySelector :: Selector
createMatchingDictionaryWithVendorID_productID_bcdDevice_deviceClass_deviceSubclass_deviceProtocol_speed_productIDArraySelector = mkSelector "createMatchingDictionaryWithVendorID:productID:bcdDevice:deviceClass:deviceSubclass:deviceProtocol:speed:productIDArray:"

-- | @Selector@ for @configureWithValue:matchInterfaces:error:@
configureWithValue_matchInterfaces_errorSelector :: Selector
configureWithValue_matchInterfaces_errorSelector = mkSelector "configureWithValue:matchInterfaces:error:"

-- | @Selector@ for @configureWithValue:error:@
configureWithValue_errorSelector :: Selector
configureWithValue_errorSelector = mkSelector "configureWithValue:error:"

-- | @Selector@ for @resetWithError:@
resetWithErrorSelector :: Selector
resetWithErrorSelector = mkSelector "resetWithError:"

-- | @Selector@ for @configurationDescriptor@
configurationDescriptorSelector :: Selector
configurationDescriptorSelector = mkSelector "configurationDescriptor"

