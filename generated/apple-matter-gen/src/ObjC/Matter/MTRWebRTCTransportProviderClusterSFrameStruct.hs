{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRWebRTCTransportProviderClusterSFrameStruct@.
module ObjC.Matter.MTRWebRTCTransportProviderClusterSFrameStruct
  ( MTRWebRTCTransportProviderClusterSFrameStruct
  , IsMTRWebRTCTransportProviderClusterSFrameStruct(..)
  , cipherSuite
  , setCipherSuite
  , baseKey
  , setBaseKey
  , kid
  , setKid
  , cipherSuiteSelector
  , setCipherSuiteSelector
  , baseKeySelector
  , setBaseKeySelector
  , kidSelector
  , setKidSelector


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

-- | @- cipherSuite@
cipherSuite :: IsMTRWebRTCTransportProviderClusterSFrameStruct mtrWebRTCTransportProviderClusterSFrameStruct => mtrWebRTCTransportProviderClusterSFrameStruct -> IO (Id NSNumber)
cipherSuite mtrWebRTCTransportProviderClusterSFrameStruct  =
    sendMsg mtrWebRTCTransportProviderClusterSFrameStruct (mkSelector "cipherSuite") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCipherSuite:@
setCipherSuite :: (IsMTRWebRTCTransportProviderClusterSFrameStruct mtrWebRTCTransportProviderClusterSFrameStruct, IsNSNumber value) => mtrWebRTCTransportProviderClusterSFrameStruct -> value -> IO ()
setCipherSuite mtrWebRTCTransportProviderClusterSFrameStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterSFrameStruct (mkSelector "setCipherSuite:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- baseKey@
baseKey :: IsMTRWebRTCTransportProviderClusterSFrameStruct mtrWebRTCTransportProviderClusterSFrameStruct => mtrWebRTCTransportProviderClusterSFrameStruct -> IO (Id NSData)
baseKey mtrWebRTCTransportProviderClusterSFrameStruct  =
    sendMsg mtrWebRTCTransportProviderClusterSFrameStruct (mkSelector "baseKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setBaseKey:@
setBaseKey :: (IsMTRWebRTCTransportProviderClusterSFrameStruct mtrWebRTCTransportProviderClusterSFrameStruct, IsNSData value) => mtrWebRTCTransportProviderClusterSFrameStruct -> value -> IO ()
setBaseKey mtrWebRTCTransportProviderClusterSFrameStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterSFrameStruct (mkSelector "setBaseKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- kid@
kid :: IsMTRWebRTCTransportProviderClusterSFrameStruct mtrWebRTCTransportProviderClusterSFrameStruct => mtrWebRTCTransportProviderClusterSFrameStruct -> IO (Id NSData)
kid mtrWebRTCTransportProviderClusterSFrameStruct  =
    sendMsg mtrWebRTCTransportProviderClusterSFrameStruct (mkSelector "kid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKid:@
setKid :: (IsMTRWebRTCTransportProviderClusterSFrameStruct mtrWebRTCTransportProviderClusterSFrameStruct, IsNSData value) => mtrWebRTCTransportProviderClusterSFrameStruct -> value -> IO ()
setKid mtrWebRTCTransportProviderClusterSFrameStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrWebRTCTransportProviderClusterSFrameStruct (mkSelector "setKid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @cipherSuite@
cipherSuiteSelector :: Selector
cipherSuiteSelector = mkSelector "cipherSuite"

-- | @Selector@ for @setCipherSuite:@
setCipherSuiteSelector :: Selector
setCipherSuiteSelector = mkSelector "setCipherSuite:"

-- | @Selector@ for @baseKey@
baseKeySelector :: Selector
baseKeySelector = mkSelector "baseKey"

-- | @Selector@ for @setBaseKey:@
setBaseKeySelector :: Selector
setBaseKeySelector = mkSelector "setBaseKey:"

-- | @Selector@ for @kid@
kidSelector :: Selector
kidSelector = mkSelector "kid"

-- | @Selector@ for @setKid:@
setKidSelector :: Selector
setKidSelector = mkSelector "setKid:"

