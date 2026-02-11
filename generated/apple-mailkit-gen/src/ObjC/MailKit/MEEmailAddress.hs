{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contain information about an email address. This can include both valid and invalid email addresses.
--
-- Generated bindings for @MEEmailAddress@.
module ObjC.MailKit.MEEmailAddress
  ( MEEmailAddress
  , IsMEEmailAddress(..)
  , new
  , init_
  , initWithRawString
  , rawString
  , addressString
  , newSelector
  , initSelector
  , initWithRawStringSelector
  , rawStringSelector
  , addressStringSelector


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

import ObjC.MailKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id MEEmailAddress)
new  =
  do
    cls' <- getRequiredClass "MEEmailAddress"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMEEmailAddress meEmailAddress => meEmailAddress -> IO (Id MEEmailAddress)
init_ meEmailAddress  =
    sendMsg meEmailAddress (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithRawString:@
initWithRawString :: (IsMEEmailAddress meEmailAddress, IsNSString rawString) => meEmailAddress -> rawString -> IO (Id MEEmailAddress)
initWithRawString meEmailAddress  rawString =
  withObjCPtr rawString $ \raw_rawString ->
      sendMsg meEmailAddress (mkSelector "initWithRawString:") (retPtr retVoid) [argPtr (castPtr raw_rawString :: Ptr ())] >>= ownedObject . castPtr

-- | The raw string for the email address.
--
-- ObjC selector: @- rawString@
rawString :: IsMEEmailAddress meEmailAddress => meEmailAddress -> IO (Id NSString)
rawString meEmailAddress  =
    sendMsg meEmailAddress (mkSelector "rawString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The simple address string portion of the raw string if it is valid. For example, the  @addressString@ of "John Appleseed <j.appleseed\@example.com>" will be "j.appleseed\@example.com".
--
-- ObjC selector: @- addressString@
addressString :: IsMEEmailAddress meEmailAddress => meEmailAddress -> IO (Id NSString)
addressString meEmailAddress  =
    sendMsg meEmailAddress (mkSelector "addressString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithRawString:@
initWithRawStringSelector :: Selector
initWithRawStringSelector = mkSelector "initWithRawString:"

-- | @Selector@ for @rawString@
rawStringSelector :: Selector
rawStringSelector = mkSelector "rawString"

-- | @Selector@ for @addressString@
addressStringSelector :: Selector
addressStringSelector = mkSelector "addressString"

