{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contains information about a message signer
--
-- Generated bindings for @MEMessageSigner@.
module ObjC.MailKit.MEMessageSigner
  ( MEMessageSigner
  , IsMEMessageSigner(..)
  , new
  , init_
  , initWithEmailAddresses_signatureLabel_context
  , emailAddresses
  , label
  , context
  , newSelector
  , initSelector
  , initWithEmailAddresses_signatureLabel_contextSelector
  , emailAddressesSelector
  , labelSelector
  , contextSelector


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
new :: IO (Id MEMessageSigner)
new  =
  do
    cls' <- getRequiredClass "MEMessageSigner"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMEMessageSigner meMessageSigner => meMessageSigner -> IO (Id MEMessageSigner)
init_ meMessageSigner  =
    sendMsg meMessageSigner (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithEmailAddresses:signatureLabel:context:@
initWithEmailAddresses_signatureLabel_context :: (IsMEMessageSigner meMessageSigner, IsNSArray emailAddresses, IsNSString label, IsNSData context) => meMessageSigner -> emailAddresses -> label -> context -> IO (Id MEMessageSigner)
initWithEmailAddresses_signatureLabel_context meMessageSigner  emailAddresses label context =
  withObjCPtr emailAddresses $ \raw_emailAddresses ->
    withObjCPtr label $ \raw_label ->
      withObjCPtr context $ \raw_context ->
          sendMsg meMessageSigner (mkSelector "initWithEmailAddresses:signatureLabel:context:") (retPtr retVoid) [argPtr (castPtr raw_emailAddresses :: Ptr ()), argPtr (castPtr raw_label :: Ptr ()), argPtr (castPtr raw_context :: Ptr ())] >>= ownedObject . castPtr

-- | Email addresses associated with the signature.
--
-- ObjC selector: @- emailAddresses@
emailAddresses :: IsMEMessageSigner meMessageSigner => meMessageSigner -> IO (Id NSArray)
emailAddresses meMessageSigner  =
    sendMsg meMessageSigner (mkSelector "emailAddresses") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The message signers label. Shown in the message header view. For instance, "John Smith".
--
-- ObjC selector: @- label@
label :: IsMEMessageSigner meMessageSigner => meMessageSigner -> IO (Id NSString)
label meMessageSigner  =
    sendMsg meMessageSigner (mkSelector "label") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The context for the message signature. This might include the signing certificate. This will be passed back to the extension for either verifying the signature or if the user wishes to view signature information.
--
-- ObjC selector: @- context@
context :: IsMEMessageSigner meMessageSigner => meMessageSigner -> IO (Id NSData)
context meMessageSigner  =
    sendMsg meMessageSigner (mkSelector "context") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithEmailAddresses:signatureLabel:context:@
initWithEmailAddresses_signatureLabel_contextSelector :: Selector
initWithEmailAddresses_signatureLabel_contextSelector = mkSelector "initWithEmailAddresses:signatureLabel:context:"

-- | @Selector@ for @emailAddresses@
emailAddressesSelector :: Selector
emailAddressesSelector = mkSelector "emailAddresses"

-- | @Selector@ for @label@
labelSelector :: Selector
labelSelector = mkSelector "label"

-- | @Selector@ for @context@
contextSelector :: Selector
contextSelector = mkSelector "context"

