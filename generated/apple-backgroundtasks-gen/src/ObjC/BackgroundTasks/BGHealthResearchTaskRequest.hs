{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A request to launch your app in the background to execute a health research task for studies a user has opted into and that can take minutes to complete.
--
-- Generated bindings for @BGHealthResearchTaskRequest@.
module ObjC.BackgroundTasks.BGHealthResearchTaskRequest
  ( BGHealthResearchTaskRequest
  , IsBGHealthResearchTaskRequest(..)
  , protectionTypeOfRequiredData
  , setProtectionTypeOfRequiredData
  , protectionTypeOfRequiredDataSelector
  , setProtectionTypeOfRequiredDataSelector


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

-- | A String indicating file protection availability required for processing.
--
-- Update this property to indicate what type of data needs to be accessible when the task is run. The default value is @NSFileProtectionCompleteUntilFirstUserAuthentication@
--
-- ObjC selector: @- protectionTypeOfRequiredData@
protectionTypeOfRequiredData :: IsBGHealthResearchTaskRequest bgHealthResearchTaskRequest => bgHealthResearchTaskRequest -> IO (Id NSString)
protectionTypeOfRequiredData bgHealthResearchTaskRequest  =
    sendMsg bgHealthResearchTaskRequest (mkSelector "protectionTypeOfRequiredData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A String indicating file protection availability required for processing.
--
-- Update this property to indicate what type of data needs to be accessible when the task is run. The default value is @NSFileProtectionCompleteUntilFirstUserAuthentication@
--
-- ObjC selector: @- setProtectionTypeOfRequiredData:@
setProtectionTypeOfRequiredData :: (IsBGHealthResearchTaskRequest bgHealthResearchTaskRequest, IsNSString value) => bgHealthResearchTaskRequest -> value -> IO ()
setProtectionTypeOfRequiredData bgHealthResearchTaskRequest  value =
  withObjCPtr value $ \raw_value ->
      sendMsg bgHealthResearchTaskRequest (mkSelector "setProtectionTypeOfRequiredData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @protectionTypeOfRequiredData@
protectionTypeOfRequiredDataSelector :: Selector
protectionTypeOfRequiredDataSelector = mkSelector "protectionTypeOfRequiredData"

-- | @Selector@ for @setProtectionTypeOfRequiredData:@
setProtectionTypeOfRequiredDataSelector :: Selector
setProtectionTypeOfRequiredDataSelector = mkSelector "setProtectionTypeOfRequiredData:"

