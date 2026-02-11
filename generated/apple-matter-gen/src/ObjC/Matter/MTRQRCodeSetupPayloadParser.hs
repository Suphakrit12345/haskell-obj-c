{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRQRCodeSetupPayloadParser@.
module ObjC.Matter.MTRQRCodeSetupPayloadParser
  ( MTRQRCodeSetupPayloadParser
  , IsMTRQRCodeSetupPayloadParser(..)
  , initWithBase38Representation
  , populatePayload
  , initWithBase38RepresentationSelector
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

-- | @- initWithBase38Representation:@
initWithBase38Representation :: (IsMTRQRCodeSetupPayloadParser mtrqrCodeSetupPayloadParser, IsNSString base38Representation) => mtrqrCodeSetupPayloadParser -> base38Representation -> IO (Id MTRQRCodeSetupPayloadParser)
initWithBase38Representation mtrqrCodeSetupPayloadParser  base38Representation =
  withObjCPtr base38Representation $ \raw_base38Representation ->
      sendMsg mtrqrCodeSetupPayloadParser (mkSelector "initWithBase38Representation:") (retPtr retVoid) [argPtr (castPtr raw_base38Representation :: Ptr ())] >>= ownedObject . castPtr

-- | @- populatePayload:@
populatePayload :: (IsMTRQRCodeSetupPayloadParser mtrqrCodeSetupPayloadParser, IsNSError error_) => mtrqrCodeSetupPayloadParser -> error_ -> IO (Id MTRSetupPayload)
populatePayload mtrqrCodeSetupPayloadParser  error_ =
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg mtrqrCodeSetupPayloadParser (mkSelector "populatePayload:") (retPtr retVoid) [argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBase38Representation:@
initWithBase38RepresentationSelector :: Selector
initWithBase38RepresentationSelector = mkSelector "initWithBase38Representation:"

-- | @Selector@ for @populatePayload:@
populatePayloadSelector :: Selector
populatePayloadSelector = mkSelector "populatePayload:"

