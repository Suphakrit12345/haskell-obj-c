{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request to launch your app in the background to execute a short refresh task.
--
-- Schedule a refresh task request to ask that the system launch your app briefly so that you can download data and keep your app's contents up-to-date. The system will fulfill this request intelligently based on system conditions and app usage.
--
-- Generated bindings for @BGAppRefreshTaskRequest@.
module ObjC.BackgroundTasks.BGAppRefreshTaskRequest
  ( BGAppRefreshTaskRequest
  , IsBGAppRefreshTaskRequest(..)
  , initWithIdentifier
  , initWithIdentifierSelector


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

import ObjC.BackgroundTasks.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Return a new refresh task request for the specified identifier.
--
-- - Parameters:     - identifier: The string identifier of the refresh task associated with the request.
--
-- ObjC selector: @- initWithIdentifier:@
initWithIdentifier :: (IsBGAppRefreshTaskRequest bgAppRefreshTaskRequest, IsNSString identifier) => bgAppRefreshTaskRequest -> identifier -> IO (Id BGAppRefreshTaskRequest)
initWithIdentifier bgAppRefreshTaskRequest  identifier =
  withObjCPtr identifier $ \raw_identifier ->
      sendMsg bgAppRefreshTaskRequest (mkSelector "initWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

