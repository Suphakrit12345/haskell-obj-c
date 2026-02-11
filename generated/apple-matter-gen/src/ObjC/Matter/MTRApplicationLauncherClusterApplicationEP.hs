{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRApplicationLauncherClusterApplicationEP@.
module ObjC.Matter.MTRApplicationLauncherClusterApplicationEP
  ( MTRApplicationLauncherClusterApplicationEP
  , IsMTRApplicationLauncherClusterApplicationEP(..)
  , application
  , setApplication
  , endpoint
  , setEndpoint
  , applicationSelector
  , setApplicationSelector
  , endpointSelector
  , setEndpointSelector


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

-- | @- application@
application :: IsMTRApplicationLauncherClusterApplicationEP mtrApplicationLauncherClusterApplicationEP => mtrApplicationLauncherClusterApplicationEP -> IO (Id MTRApplicationLauncherClusterApplicationStruct)
application mtrApplicationLauncherClusterApplicationEP  =
    sendMsg mtrApplicationLauncherClusterApplicationEP (mkSelector "application") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setApplication:@
setApplication :: (IsMTRApplicationLauncherClusterApplicationEP mtrApplicationLauncherClusterApplicationEP, IsMTRApplicationLauncherClusterApplicationStruct value) => mtrApplicationLauncherClusterApplicationEP -> value -> IO ()
setApplication mtrApplicationLauncherClusterApplicationEP  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrApplicationLauncherClusterApplicationEP (mkSelector "setApplication:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- endpoint@
endpoint :: IsMTRApplicationLauncherClusterApplicationEP mtrApplicationLauncherClusterApplicationEP => mtrApplicationLauncherClusterApplicationEP -> IO (Id NSNumber)
endpoint mtrApplicationLauncherClusterApplicationEP  =
    sendMsg mtrApplicationLauncherClusterApplicationEP (mkSelector "endpoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setEndpoint:@
setEndpoint :: (IsMTRApplicationLauncherClusterApplicationEP mtrApplicationLauncherClusterApplicationEP, IsNSNumber value) => mtrApplicationLauncherClusterApplicationEP -> value -> IO ()
setEndpoint mtrApplicationLauncherClusterApplicationEP  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrApplicationLauncherClusterApplicationEP (mkSelector "setEndpoint:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @application@
applicationSelector :: Selector
applicationSelector = mkSelector "application"

-- | @Selector@ for @setApplication:@
setApplicationSelector :: Selector
setApplicationSelector = mkSelector "setApplication:"

-- | @Selector@ for @endpoint@
endpointSelector :: Selector
endpointSelector = mkSelector "endpoint"

-- | @Selector@ for @setEndpoint:@
setEndpointSelector :: Selector
setEndpointSelector = mkSelector "setEndpoint:"

