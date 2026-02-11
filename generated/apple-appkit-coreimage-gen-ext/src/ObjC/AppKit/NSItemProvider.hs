{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSItemProvider@.
module ObjC.AppKit.NSItemProvider
  ( NSItemProvider
  , IsNSItemProvider(..)
  , registerCloudKitShareWithPreparationHandler
  , registerCloudKitShareWithPreparationHandlerSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes

-- | Use this method when you want to share a collection of CKRecords but don't currently have a CKShare. When the preparationHandler is called, you should create a new CKShare with the appropriate root CKRecord. After ensuring the share and all records have been saved to the server, invoke the preparationCompletionHandler with either the resulting CKShare and its CKContainer, or an NSError if saving failed. Invoking the service with a CKShare registered with this method will prompt the user to start sharing.
--
-- ObjC selector: @- registerCloudKitShareWithPreparationHandler:@
registerCloudKitShareWithPreparationHandler :: IsNSItemProvider nsItemProvider => nsItemProvider -> Ptr () -> IO ()
registerCloudKitShareWithPreparationHandler nsItemProvider  preparationHandler =
    sendMsg nsItemProvider (mkSelector "registerCloudKitShareWithPreparationHandler:") retVoid [argPtr (castPtr preparationHandler :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @registerCloudKitShareWithPreparationHandler:@
registerCloudKitShareWithPreparationHandlerSelector :: Selector
registerCloudKitShareWithPreparationHandlerSelector = mkSelector "registerCloudKitShareWithPreparationHandler:"

