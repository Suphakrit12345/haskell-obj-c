{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommissioningOperation@.
module ObjC.Matter.MTRCommissioningOperation
  ( MTRCommissioningOperation
  , IsMTRCommissioningOperation(..)
  , init_
  , new
  , initWithParameters_setupPayload_delegate_queue
  , startWithController
  , stop
  , matchedPayload
  , initSelector
  , newSelector
  , initWithParameters_setupPayload_delegate_queueSelector
  , startWithControllerSelector
  , stopSelector
  , matchedPayloadSelector


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

-- | Initialized via initWithParameters:setupPayload:delegate:queue:
--
-- ObjC selector: @- init@
init_ :: IsMTRCommissioningOperation mtrCommissioningOperation => mtrCommissioningOperation -> IO (Id MTRCommissioningOperation)
init_ mtrCommissioningOperation  =
    sendMsg mtrCommissioningOperation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id MTRCommissioningOperation)
new  =
  do
    cls' <- getRequiredClass "MTRCommissioningOperation"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Prepare to commission a device with the given parameters and the given setup payload (QR code, manual pairing code, etc).  Returns nil if the payload is not valid.
--
-- The deviceAttestationDelegate property of MTRCommissioningParameters will be ignored. Device attestation notifications will be delivered to the MTRCommissioningDelegate instead.  The failSafeTimeout property of MTRCommissioningParameters will be respected.
--
-- The provided delegate will be notified about various things as commissioning proceeds.  The calls into the delegate will happen on the provided queue.
--
-- Modifying the parameters after this call will have no effect on the behavior of the MTRCommissioningOperation.
--
-- ObjC selector: @- initWithParameters:setupPayload:delegate:queue:@
initWithParameters_setupPayload_delegate_queue :: (IsMTRCommissioningOperation mtrCommissioningOperation, IsMTRCommissioningParameters parameters, IsNSString payload, IsNSObject queue) => mtrCommissioningOperation -> parameters -> payload -> RawId -> queue -> IO (Id MTRCommissioningOperation)
initWithParameters_setupPayload_delegate_queue mtrCommissioningOperation  parameters payload delegate queue =
  withObjCPtr parameters $ \raw_parameters ->
    withObjCPtr payload $ \raw_payload ->
      withObjCPtr queue $ \raw_queue ->
          sendMsg mtrCommissioningOperation (mkSelector "initWithParameters:setupPayload:delegate:queue:") (retPtr retVoid) [argPtr (castPtr raw_parameters :: Ptr ()), argPtr (castPtr raw_payload :: Ptr ()), argPtr (castPtr (unRawId delegate) :: Ptr ()), argPtr (castPtr raw_queue :: Ptr ())] >>= ownedObject . castPtr

-- | Start commissioning with the given controller (which identifies the fabric the commissionee should be commissioned into).  The delegate will be notified if there are any failures.
--
-- ObjC selector: @- startWithController:@
startWithController :: (IsMTRCommissioningOperation mtrCommissioningOperation, IsMTRDeviceController controller) => mtrCommissioningOperation -> controller -> IO ()
startWithController mtrCommissioningOperation  controller =
  withObjCPtr controller $ \raw_controller ->
      sendMsg mtrCommissioningOperation (mkSelector "startWithController:") retVoid [argPtr (castPtr raw_controller :: Ptr ())]

-- | Stop commissioning.  This will typically result in commissioning:failedWithError: callbacks to delegates.
--
-- Returns YES if this commissioning was still in-progress and has now been stopped; returns NO if this commissioning wasn't in-progress.
--
-- Note that this can return NO while there are still pending async calls to delegate callbacks for the end of the commissioning.
--
-- ObjC selector: @- stop@
stop :: IsMTRCommissioningOperation mtrCommissioningOperation => mtrCommissioningOperation -> IO Bool
stop mtrCommissioningOperation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mtrCommissioningOperation (mkSelector "stop") retCULong []

-- | If not nil, the payload (from possibly multiple payloads represented by the provided setupPayload) that represents the commissionee we successfully established PASE with.  This will only be non-nil after successful PASE establishment.
--
-- ObjC selector: @- matchedPayload@
matchedPayload :: IsMTRCommissioningOperation mtrCommissioningOperation => mtrCommissioningOperation -> IO (Id MTRSetupPayload)
matchedPayload mtrCommissioningOperation  =
    sendMsg mtrCommissioningOperation (mkSelector "matchedPayload") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @initWithParameters:setupPayload:delegate:queue:@
initWithParameters_setupPayload_delegate_queueSelector :: Selector
initWithParameters_setupPayload_delegate_queueSelector = mkSelector "initWithParameters:setupPayload:delegate:queue:"

-- | @Selector@ for @startWithController:@
startWithControllerSelector :: Selector
startWithControllerSelector = mkSelector "startWithController:"

-- | @Selector@ for @stop@
stopSelector :: Selector
stopSelector = mkSelector "stop"

-- | @Selector@ for @matchedPayload@
matchedPayloadSelector :: Selector
matchedPayloadSelector = mkSelector "matchedPayload"

