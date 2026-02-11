{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Information read from the commissionee device during commissioning.
--
-- Generated bindings for @MTRCommissioneeInfo@.
module ObjC.Matter.MTRCommissioneeInfo
  ( MTRCommissioneeInfo
  , IsMTRCommissioneeInfo(..)
  , productIdentity
  , endpointsById
  , rootEndpoint
  , attributes
  , networkInterfaces
  , productIdentitySelector
  , endpointsByIdSelector
  , rootEndpointSelector
  , attributesSelector
  , networkInterfacesSelector


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

-- | The product identity (VID / PID) of the commissionee.
--
-- ObjC selector: @- productIdentity@
productIdentity :: IsMTRCommissioneeInfo mtrCommissioneeInfo => mtrCommissioneeInfo -> IO (Id MTRProductIdentity)
productIdentity mtrCommissioneeInfo  =
    sendMsg mtrCommissioneeInfo (mkSelector "productIdentity") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Endpoint information for all endpoints of the commissionee. Will be present only if readEndpointInformation is set to YES on MTRCommissioningParameters.
--
-- Use @rootEndpoint@ and @-[MTREndpointInfo children]@ to traverse endpoints in composition order.
--
-- ObjC selector: @- endpointsById@
endpointsById :: IsMTRCommissioneeInfo mtrCommissioneeInfo => mtrCommissioneeInfo -> IO (Id NSDictionary)
endpointsById mtrCommissioneeInfo  =
    sendMsg mtrCommissioneeInfo (mkSelector "endpointsById") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Endpoint information for the root endpoint of the commissionee. Will be present only if readEndpointInformation is set to YES on MTRCommissioningParameters.
--
-- ObjC selector: @- rootEndpoint@
rootEndpoint :: IsMTRCommissioneeInfo mtrCommissioneeInfo => mtrCommissioneeInfo -> IO (Id MTREndpointInfo)
rootEndpoint mtrCommissioneeInfo  =
    sendMsg mtrCommissioneeInfo (mkSelector "rootEndpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Attributes that were read from the commissionee.  This will contain the following, if they are available:
--
-- 1) The attributes in extraAttributesToRead on MTRCommissioningParameters. 2) The FeatureMap attributes of all Network Commissioning clusters on the commissionee.
--
-- ObjC selector: @- attributes@
attributes :: IsMTRCommissioneeInfo mtrCommissioneeInfo => mtrCommissioneeInfo -> IO (Id NSDictionary)
attributes mtrCommissioneeInfo  =
    sendMsg mtrCommissioneeInfo (mkSelector "attributes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Network interfaces the commissionee has.  The array will be empty if there are no network interfaces exposed on the commissionee.
--
-- ObjC selector: @- networkInterfaces@
networkInterfaces :: IsMTRCommissioneeInfo mtrCommissioneeInfo => mtrCommissioneeInfo -> IO (Id NSArray)
networkInterfaces mtrCommissioneeInfo  =
    sendMsg mtrCommissioneeInfo (mkSelector "networkInterfaces") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @productIdentity@
productIdentitySelector :: Selector
productIdentitySelector = mkSelector "productIdentity"

-- | @Selector@ for @endpointsById@
endpointsByIdSelector :: Selector
endpointsByIdSelector = mkSelector "endpointsById"

-- | @Selector@ for @rootEndpoint@
rootEndpointSelector :: Selector
rootEndpointSelector = mkSelector "rootEndpoint"

-- | @Selector@ for @attributes@
attributesSelector :: Selector
attributesSelector = mkSelector "attributes"

-- | @Selector@ for @networkInterfaces@
networkInterfacesSelector :: Selector
networkInterfacesSelector = mkSelector "networkInterfaces"

