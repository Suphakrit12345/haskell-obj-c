{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRManualSetupPayloadParser@.
module ObjC.Matter.MTRManualSetupPayloadParser
  ( MTRManualSetupPayloadParser
  , IsMTRManualSetupPayloadParser(..)
  , initWithDecimalStringRepresentation
  , populatePayload
  , initWithDecimalStringRepresentationSelector
  , populatePayloadSelector


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

-- | @- initWithDecimalStringRepresentation:@
initWithDecimalStringRepresentation :: (IsMTRManualSetupPayloadParser mtrManualSetupPayloadParser, IsNSString decimalStringRepresentation) => mtrManualSetupPayloadParser -> decimalStringRepresentation -> IO (Id MTRManualSetupPayloadParser)
initWithDecimalStringRepresentation mtrManualSetupPayloadParser  decimalStringRepresentation =
  withObjCPtr decimalStringRepresentation $ \raw_decimalStringRepresentation ->
      sendMsg mtrManualSetupPayloadParser (mkSelector "initWithDecimalStringRepresentation:") (retPtr retVoid) [argPtr (castPtr raw_decimalStringRepresentation :: Ptr ())] >>= ownedObject . castPtr

-- | @- populatePayload:@
populatePayload :: (IsMTRManualSetupPayloadParser mtrManualSetupPayloadParser, IsNSError error_) => mtrManualSetupPayloadParser -> error_ -> IO (Id MTRSetupPayload)
populatePayload mtrManualSetupPayloadParser  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg mtrManualSetupPayloadParser (mkSelector "populatePayload:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithDecimalStringRepresentation:@
initWithDecimalStringRepresentationSelector :: Selector
initWithDecimalStringRepresentationSelector = mkSelector "initWithDecimalStringRepresentation:"

-- | @Selector@ for @populatePayload:@
populatePayloadSelector :: Selector
populatePayloadSelector = mkSelector "populatePayload:"

