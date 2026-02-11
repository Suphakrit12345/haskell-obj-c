{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contains information about a decoded message
--
-- Generated bindings for @MEDecodedMessage@.
module ObjC.MailKit.MEDecodedMessage
  ( MEDecodedMessage
  , IsMEDecodedMessage(..)
  , new
  , init_
  , initWithData_securityInformation_context
  , initWithData_securityInformation_context_banner
  , rawData
  , securityInformation
  , context
  , banner
  , newSelector
  , initSelector
  , initWithData_securityInformation_contextSelector
  , initWithData_securityInformation_context_bannerSelector
  , rawDataSelector
  , securityInformationSelector
  , contextSelector
  , bannerSelector


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
new :: IO (Id MEDecodedMessage)
new  =
  do
    cls' <- getRequiredClass "MEDecodedMessage"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMEDecodedMessage meDecodedMessage => meDecodedMessage -> IO (Id MEDecodedMessage)
init_ meDecodedMessage  =
    sendMsg meDecodedMessage (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithData:securityInformation:context:@
initWithData_securityInformation_context :: (IsMEDecodedMessage meDecodedMessage, IsNSData rawData, IsMEMessageSecurityInformation securityInformation, IsNSData context) => meDecodedMessage -> rawData -> securityInformation -> context -> IO (Id MEDecodedMessage)
initWithData_securityInformation_context meDecodedMessage  rawData securityInformation context =
  withObjCPtr rawData $ \raw_rawData ->
    withObjCPtr securityInformation $ \raw_securityInformation ->
      withObjCPtr context $ \raw_context ->
          sendMsg meDecodedMessage (mkSelector "initWithData:securityInformation:context:") (retPtr retVoid) [argPtr (castPtr raw_rawData :: Ptr ()), argPtr (castPtr raw_securityInformation :: Ptr ()), argPtr (castPtr raw_context :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithData:securityInformation:context:banner:@
initWithData_securityInformation_context_banner :: (IsMEDecodedMessage meDecodedMessage, IsNSData rawData, IsMEMessageSecurityInformation securityInformation, IsNSData context, IsMEDecodedMessageBanner banner) => meDecodedMessage -> rawData -> securityInformation -> context -> banner -> IO (Id MEDecodedMessage)
initWithData_securityInformation_context_banner meDecodedMessage  rawData securityInformation context banner =
  withObjCPtr rawData $ \raw_rawData ->
    withObjCPtr securityInformation $ \raw_securityInformation ->
      withObjCPtr context $ \raw_context ->
        withObjCPtr banner $ \raw_banner ->
            sendMsg meDecodedMessage (mkSelector "initWithData:securityInformation:context:banner:") (retPtr retVoid) [argPtr (castPtr raw_rawData :: Ptr ()), argPtr (castPtr raw_securityInformation :: Ptr ()), argPtr (castPtr raw_context :: Ptr ()), argPtr (castPtr raw_banner :: Ptr ())] >>= ownedObject . castPtr

-- | The decoded MIME data for the message The decoded data should not be encrypted or contain any signatures that were decoded. The @rawData@ here should only contain MIME parts that a standard email parser can decode without needing to decrypt. All information on the encryption and signature status should be defined in @securityInformation.@ If the message is unable to be decrypted this should be left nil and an error message will be displayed to the user.
--
-- ObjC selector: @- rawData@
rawData :: IsMEDecodedMessage meDecodedMessage => meDecodedMessage -> IO (Id NSData)
rawData meDecodedMessage  =
    sendMsg meDecodedMessage (mkSelector "rawData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The security information for whether or not the message was signed, encrypted, or had an errors in decoding.
--
-- ObjC selector: @- securityInformation@
securityInformation :: IsMEDecodedMessage meDecodedMessage => meDecodedMessage -> IO (Id MEMessageSecurityInformation)
securityInformation meDecodedMessage  =
    sendMsg meDecodedMessage (mkSelector "securityInformation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The context for the decoded message. This will be passed back to the extension when Mail loads the extension's custom view controller for the message.
--
-- ObjC selector: @- context@
context :: IsMEDecodedMessage meDecodedMessage => meDecodedMessage -> IO (Id NSData)
context meDecodedMessage  =
    sendMsg meDecodedMessage (mkSelector "context") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Suggestion information used to populate a suggestion banner at the top of the message view. Clicking on the action associated with the suggestion banner will present the extension's view controller for the provided message context.
--
-- ObjC selector: @- banner@
banner :: IsMEDecodedMessage meDecodedMessage => meDecodedMessage -> IO (Id MEDecodedMessageBanner)
banner meDecodedMessage  =
    sendMsg meDecodedMessage (mkSelector "banner") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithData:securityInformation:context:@
initWithData_securityInformation_contextSelector :: Selector
initWithData_securityInformation_contextSelector = mkSelector "initWithData:securityInformation:context:"

-- | @Selector@ for @initWithData:securityInformation:context:banner:@
initWithData_securityInformation_context_bannerSelector :: Selector
initWithData_securityInformation_context_bannerSelector = mkSelector "initWithData:securityInformation:context:banner:"

-- | @Selector@ for @rawData@
rawDataSelector :: Selector
rawDataSelector = mkSelector "rawData"

-- | @Selector@ for @securityInformation@
securityInformationSelector :: Selector
securityInformationSelector = mkSelector "securityInformation"

-- | @Selector@ for @context@
contextSelector :: Selector
contextSelector = mkSelector "context"

-- | @Selector@ for @banner@
bannerSelector :: Selector
bannerSelector = mkSelector "banner"

