{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @JRSRenderServer@.
module ObjC.JavaRuntimeSupport.JRSRenderServer
  ( JRSRenderServer
  , IsJRSRenderServer(..)
  , startRenderServer
  , sendRenderServer
  , recieveRenderServer
  , startRenderServerSelector
  , sendRenderServerSelector
  , recieveRenderServerSelector


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

import ObjC.JavaRuntimeSupport.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ startRenderServer@
startRenderServer :: IO CUInt
startRenderServer  =
  do
    cls' <- getRequiredClass "JRSRenderServer"
    sendClassMsg cls' (mkSelector "startRenderServer") retCUInt []

-- | @+ sendRenderServer:@
sendRenderServer :: CUInt -> IO (Id NSString)
sendRenderServer serverPort =
  do
    cls' <- getRequiredClass "JRSRenderServer"
    sendClassMsg cls' (mkSelector "sendRenderServer:") (retPtr retVoid) [argCUInt serverPort] >>= retainedObject . castPtr

-- | @+ recieveRenderServer:@
recieveRenderServer :: IsNSString serverName => serverName -> IO CUInt
recieveRenderServer serverName =
  do
    cls' <- getRequiredClass "JRSRenderServer"
    withObjCPtr serverName $ \raw_serverName ->
      sendClassMsg cls' (mkSelector "recieveRenderServer:") retCUInt [argPtr (castPtr raw_serverName :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @startRenderServer@
startRenderServerSelector :: Selector
startRenderServerSelector = mkSelector "startRenderServer"

-- | @Selector@ for @sendRenderServer:@
sendRenderServerSelector :: Selector
sendRenderServerSelector = mkSelector "sendRenderServer:"

-- | @Selector@ for @recieveRenderServer:@
recieveRenderServerSelector :: Selector
recieveRenderServerSelector = mkSelector "recieveRenderServer:"

