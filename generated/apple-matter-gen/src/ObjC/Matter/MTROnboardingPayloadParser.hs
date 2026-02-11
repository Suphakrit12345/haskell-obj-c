{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTROnboardingPayloadParser@.
module ObjC.Matter.MTROnboardingPayloadParser
  ( MTROnboardingPayloadParser
  , IsMTROnboardingPayloadParser(..)
  , setupPayloadForOnboardingPayload_error
  , setupPayloadForOnboardingPayload_errorSelector


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

-- | @+ setupPayloadForOnboardingPayload:error:@
setupPayloadForOnboardingPayload_error :: (IsNSString onboardingPayload, IsNSError error_) => onboardingPayload -> error_ -> IO (Id MTRSetupPayload)
setupPayloadForOnboardingPayload_error onboardingPayload error_ =
  do
    cls' <- getRequiredClass "MTROnboardingPayloadParser"
    withObjCPtr onboardingPayload $ \raw_onboardingPayload ->
      withObjCPtr error_ $ \raw_error_ ->
        sendClassMsg cls' (mkSelector "setupPayloadForOnboardingPayload:error:") (retPtr retVoid) [argPtr (castPtr raw_onboardingPayload :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @setupPayloadForOnboardingPayload:error:@
setupPayloadForOnboardingPayload_errorSelector :: Selector
setupPayloadForOnboardingPayload_errorSelector = mkSelector "setupPayloadForOnboardingPayload:error:"

