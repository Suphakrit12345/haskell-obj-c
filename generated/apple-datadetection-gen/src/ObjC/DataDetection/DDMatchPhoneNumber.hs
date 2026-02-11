{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | An object that contains a phone number that the data detection system matches.
--
-- The DataDetection framework returns a phone number match in a @DDMatchPhoneNumber@ object, which contains a phone number, and optionally a label that categorizes the phone number.
--
-- Generated bindings for @DDMatchPhoneNumber@.
module ObjC.DataDetection.DDMatchPhoneNumber
  ( DDMatchPhoneNumber
  , IsDDMatchPhoneNumber(..)
  , phoneNumber
  , label
  , phoneNumberSelector
  , labelSelector


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

import ObjC.DataDetection.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | A string that represents a phone number.
--
-- ObjC selector: @- phoneNumber@
phoneNumber :: IsDDMatchPhoneNumber ddMatchPhoneNumber => ddMatchPhoneNumber -> IO (Id NSString)
phoneNumber ddMatchPhoneNumber  =
    sendMsg ddMatchPhoneNumber (mkSelector "phoneNumber") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A string that categorizes a phone number, such as Home or Work.
--
-- ObjC selector: @- label@
label :: IsDDMatchPhoneNumber ddMatchPhoneNumber => ddMatchPhoneNumber -> IO (Id NSString)
label ddMatchPhoneNumber  =
    sendMsg ddMatchPhoneNumber (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @phoneNumber@
phoneNumberSelector :: Selector
phoneNumberSelector = mkSelector "phoneNumber"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

