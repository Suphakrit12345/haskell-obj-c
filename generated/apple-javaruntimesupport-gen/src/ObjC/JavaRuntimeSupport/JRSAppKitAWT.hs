{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @JRSAppKitAWT@.
module ObjC.JavaRuntimeSupport.JRSAppKitAWT
  ( JRSAppKitAWT
  , IsJRSAppKitAWT(..)
  , awtAppDelegate
  , registerAWTAppWithOptions
  , markAppIsDaemon
  , awtAppDelegateSelector
  , registerAWTAppWithOptionsSelector
  , markAppIsDaemonSelector


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

-- | @+ awtAppDelegate@
awtAppDelegate :: IO RawId
awtAppDelegate  =
  do
    cls' <- getRequiredClass "JRSAppKitAWT"
    fmap (RawId . castPtr) $ sendClassMsg cls' (mkSelector "awtAppDelegate") (retPtr retVoid) []

-- | @+ registerAWTAppWithOptions:@
registerAWTAppWithOptions :: IsNSDictionary options => options -> IO ()
registerAWTAppWithOptions options =
  do
    cls' <- getRequiredClass "JRSAppKitAWT"
    withObjCPtr options $ \raw_options ->
      sendClassMsg cls' (mkSelector "registerAWTAppWithOptions:") retVoid [argPtr (castPtr raw_options :: Ptr ())]

-- | @+ markAppIsDaemon@
markAppIsDaemon :: IO Bool
markAppIsDaemon  =
  do
    cls' <- getRequiredClass "JRSAppKitAWT"
    fmap ((/= 0) :: CULong -> Bool) $ sendClassMsg cls' (mkSelector "markAppIsDaemon") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @awtAppDelegate@
awtAppDelegateSelector :: Selector
awtAppDelegateSelector = mkSelector "awtAppDelegate"

-- | @Selector@ for @registerAWTAppWithOptions:@
registerAWTAppWithOptionsSelector :: Selector
registerAWTAppWithOptionsSelector = mkSelector "registerAWTAppWithOptions:"

-- | @Selector@ for @markAppIsDaemon@
markAppIsDaemonSelector :: Selector
markAppIsDaemonSelector = mkSelector "markAppIsDaemon"

