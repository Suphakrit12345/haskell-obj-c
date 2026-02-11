{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRCommissionerControlClusterReverseOpenCommissioningWindowParams@.
module ObjC.Matter.MTRCommissionerControlClusterReverseOpenCommissioningWindowParams
  ( MTRCommissionerControlClusterReverseOpenCommissioningWindowParams
  , IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams(..)
  , initWithResponseValue_error
  , commissioningTimeout
  , setCommissioningTimeout
  , pakePasscodeVerifier
  , setPakePasscodeVerifier
  , discriminator
  , setDiscriminator
  , iterations
  , setIterations
  , salt
  , setSalt
  , initWithResponseValue_errorSelector
  , commissioningTimeoutSelector
  , setCommissioningTimeoutSelector
  , pakePasscodeVerifierSelector
  , setPakePasscodeVerifierSelector
  , discriminatorSelector
  , setDiscriminatorSelector
  , iterationsSelector
  , setIterationsSelector
  , saltSelector
  , setSaltSelector


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

-- | Initialize an MTRCommissionerControlClusterReverseOpenCommissioningWindowParams with a response-value dictionary of the sort that MTRDeviceResponseHandler would receive.
--
-- Will return nil and hand out an error if the response-value dictionary is not a command data response or is not the right command response.
--
-- Will return nil and hand out an error if the data response does not match the known schema for this command.
--
-- ObjC selector: @- initWithResponseValue:error:@
initWithResponseValue_error :: (IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams, IsNSDictionary responseValue, IsNSError error_) => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> responseValue -> error_ -> IO (Id MTRCommissionerControlClusterReverseOpenCommissioningWindowParams)
initWithResponseValue_error mtrCommissionerControlClusterReverseOpenCommissioningWindowParams  responseValue error_ =
  withObjCPtr responseValue $ \raw_responseValue ->
    withObjCPtr error_ $ \raw_error_ ->
        sendMsg mtrCommissionerControlClusterReverseOpenCommissioningWindowParams (mkSelector "initWithResponseValue:error:") (retPtr retVoid) [argPtr (castPtr raw_responseValue :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= ownedObject . castPtr

-- | @- commissioningTimeout@
commissioningTimeout :: IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> IO (Id NSNumber)
commissioningTimeout mtrCommissionerControlClusterReverseOpenCommissioningWindowParams  =
    sendMsg mtrCommissionerControlClusterReverseOpenCommissioningWindowParams (mkSelector "commissioningTimeout") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCommissioningTimeout:@
setCommissioningTimeout :: (IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams, IsNSNumber value) => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> value -> IO ()
setCommissioningTimeout mtrCommissionerControlClusterReverseOpenCommissioningWindowParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissionerControlClusterReverseOpenCommissioningWindowParams (mkSelector "setCommissioningTimeout:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- pakePasscodeVerifier@
pakePasscodeVerifier :: IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> IO (Id NSData)
pakePasscodeVerifier mtrCommissionerControlClusterReverseOpenCommissioningWindowParams  =
    sendMsg mtrCommissionerControlClusterReverseOpenCommissioningWindowParams (mkSelector "pakePasscodeVerifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPakePasscodeVerifier:@
setPakePasscodeVerifier :: (IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams, IsNSData value) => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> value -> IO ()
setPakePasscodeVerifier mtrCommissionerControlClusterReverseOpenCommissioningWindowParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissionerControlClusterReverseOpenCommissioningWindowParams (mkSelector "setPakePasscodeVerifier:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- discriminator@
discriminator :: IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> IO (Id NSNumber)
discriminator mtrCommissionerControlClusterReverseOpenCommissioningWindowParams  =
    sendMsg mtrCommissionerControlClusterReverseOpenCommissioningWindowParams (mkSelector "discriminator") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setDiscriminator:@
setDiscriminator :: (IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams, IsNSNumber value) => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> value -> IO ()
setDiscriminator mtrCommissionerControlClusterReverseOpenCommissioningWindowParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissionerControlClusterReverseOpenCommissioningWindowParams (mkSelector "setDiscriminator:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- iterations@
iterations :: IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> IO (Id NSNumber)
iterations mtrCommissionerControlClusterReverseOpenCommissioningWindowParams  =
    sendMsg mtrCommissionerControlClusterReverseOpenCommissioningWindowParams (mkSelector "iterations") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setIterations:@
setIterations :: (IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams, IsNSNumber value) => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> value -> IO ()
setIterations mtrCommissionerControlClusterReverseOpenCommissioningWindowParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissionerControlClusterReverseOpenCommissioningWindowParams (mkSelector "setIterations:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- salt@
salt :: IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> IO (Id NSData)
salt mtrCommissionerControlClusterReverseOpenCommissioningWindowParams  =
    sendMsg mtrCommissionerControlClusterReverseOpenCommissioningWindowParams (mkSelector "salt") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSalt:@
setSalt :: (IsMTRCommissionerControlClusterReverseOpenCommissioningWindowParams mtrCommissionerControlClusterReverseOpenCommissioningWindowParams, IsNSData value) => mtrCommissionerControlClusterReverseOpenCommissioningWindowParams -> value -> IO ()
setSalt mtrCommissionerControlClusterReverseOpenCommissioningWindowParams  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrCommissionerControlClusterReverseOpenCommissioningWindowParams (mkSelector "setSalt:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithResponseValue:error:@
initWithResponseValue_errorSelector :: Selector
initWithResponseValue_errorSelector = mkSelector "initWithResponseValue:error:"

-- | @Selector@ for @commissioningTimeout@
commissioningTimeoutSelector :: Selector
commissioningTimeoutSelector = mkSelector "commissioningTimeout"

-- | @Selector@ for @setCommissioningTimeout:@
setCommissioningTimeoutSelector :: Selector
setCommissioningTimeoutSelector = mkSelector "setCommissioningTimeout:"

-- | @Selector@ for @pakePasscodeVerifier@
pakePasscodeVerifierSelector :: Selector
pakePasscodeVerifierSelector = mkSelector "pakePasscodeVerifier"

-- | @Selector@ for @setPakePasscodeVerifier:@
setPakePasscodeVerifierSelector :: Selector
setPakePasscodeVerifierSelector = mkSelector "setPakePasscodeVerifier:"

-- | @Selector@ for @discriminator@
discriminatorSelector :: Selector
discriminatorSelector = mkSelector "discriminator"

-- | @Selector@ for @setDiscriminator:@
setDiscriminatorSelector :: Selector
setDiscriminatorSelector = mkSelector "setDiscriminator:"

-- | @Selector@ for @iterations@
iterationsSelector :: Selector
iterationsSelector = mkSelector "iterations"

-- | @Selector@ for @setIterations:@
setIterationsSelector :: Selector
setIterationsSelector = mkSelector "setIterations:"

-- | @Selector@ for @salt@
saltSelector :: Selector
saltSelector = mkSelector "salt"

-- | @Selector@ for @setSalt:@
setSaltSelector :: Selector
setSaltSelector = mkSelector "setSalt:"

