{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommissionableBrowserResult@.
module ObjC.Matter.MTRCommissionableBrowserResult
  ( MTRCommissionableBrowserResult
  , IsMTRCommissionableBrowserResult(..)
  , instanceName
  , vendorID
  , productID
  , discriminator
  , commissioningMode
  , instanceNameSelector
  , vendorIDSelector
  , productIDSelector
  , discriminatorSelector
  , commissioningModeSelector


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

-- | For a node advertising over DNS-SD, the instance name is a dynamic, pseudo-randomly selected, 64-bit temporary unique identifier, expressed as a fixed-length sixteen-character hexadecimal string, encoded as ASCII text using capital letters.
--
-- For a node advertising over Bluetooth Low Energy, the instance name is always "BLE".
--
-- ObjC selector: @- instanceName@
instanceName :: IsMTRCommissionableBrowserResult mtrCommissionableBrowserResult => mtrCommissionableBrowserResult -> IO (Id NSString)
instanceName mtrCommissionableBrowserResult  =
    sendMsg mtrCommissionableBrowserResult (mkSelector "instanceName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A 16-bit unsigned value identifying the device manufacturer.
--
-- ObjC selector: @- vendorID@
vendorID :: IsMTRCommissionableBrowserResult mtrCommissionableBrowserResult => mtrCommissionableBrowserResult -> IO (Id NSNumber)
vendorID mtrCommissionableBrowserResult  =
    sendMsg mtrCommissionableBrowserResult (mkSelector "vendorID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A 16-bit unsigned value identifying the product.
--
-- ObjC selector: @- productID@
productID :: IsMTRCommissionableBrowserResult mtrCommissionableBrowserResult => mtrCommissionableBrowserResult -> IO (Id NSNumber)
productID mtrCommissionableBrowserResult  =
    sendMsg mtrCommissionableBrowserResult (mkSelector "productID") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A 12-bit value matching the field of the same name in MTRSetupPayload.
--
-- ObjC selector: @- discriminator@
discriminator :: IsMTRCommissionableBrowserResult mtrCommissionableBrowserResult => mtrCommissionableBrowserResult -> IO (Id NSNumber)
discriminator mtrCommissionableBrowserResult  =
    sendMsg mtrCommissionableBrowserResult (mkSelector "discriminator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A boolean indicating whether the device has a commissioning window open.
--
-- ObjC selector: @- commissioningMode@
commissioningMode :: IsMTRCommissionableBrowserResult mtrCommissionableBrowserResult => mtrCommissionableBrowserResult -> IO Bool
commissioningMode mtrCommissionableBrowserResult  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrCommissionableBrowserResult (mkSelector "commissioningMode") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @instanceName@
instanceNameSelector :: Selector
instanceNameSelector = mkSelector "instanceName"

-- | @Selector@ for @vendorID@
vendorIDSelector :: Selector
vendorIDSelector = mkSelector "vendorID"

-- | @Selector@ for @productID@
productIDSelector :: Selector
productIDSelector = mkSelector "productID"

-- | @Selector@ for @discriminator@
discriminatorSelector :: Selector
discriminatorSelector = mkSelector "discriminator"

-- | @Selector@ for @commissioningMode@
commissioningModeSelector :: Selector
commissioningModeSelector = mkSelector "commissioningMode"

