{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contains information about any security measures that will be applied to a mail message when it is sent or any errrors that occurred while verifying security status.
--
-- Generated bindings for @MEOutgoingMessageEncodingStatus@.
module ObjC.MailKit.MEOutgoingMessageEncodingStatus
  ( MEOutgoingMessageEncodingStatus
  , IsMEOutgoingMessageEncodingStatus(..)
  , new
  , init_
  , initWithCanSign_canEncrypt_securityError_addressesFailingEncryption
  , canSign
  , canEncrypt
  , securityError
  , addressesFailingEncryption
  , newSelector
  , initSelector
  , initWithCanSign_canEncrypt_securityError_addressesFailingEncryptionSelector
  , canSignSelector
  , canEncryptSelector
  , securityErrorSelector
  , addressesFailingEncryptionSelector


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
new :: IO (Id MEOutgoingMessageEncodingStatus)
new  =
  do
    cls' <- getRequiredClass "MEOutgoingMessageEncodingStatus"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMEOutgoingMessageEncodingStatus meOutgoingMessageEncodingStatus => meOutgoingMessageEncodingStatus -> IO (Id MEOutgoingMessageEncodingStatus)
init_ meOutgoingMessageEncodingStatus  =
    sendMsg meOutgoingMessageEncodingStatus (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCanSign:canEncrypt:securityError:addressesFailingEncryption:@
initWithCanSign_canEncrypt_securityError_addressesFailingEncryption :: (IsMEOutgoingMessageEncodingStatus meOutgoingMessageEncodingStatus, IsNSError securityError, IsNSArray addressesFailingEncryption) => meOutgoingMessageEncodingStatus -> Bool -> Bool -> securityError -> addressesFailingEncryption -> IO (Id MEOutgoingMessageEncodingStatus)
initWithCanSign_canEncrypt_securityError_addressesFailingEncryption meOutgoingMessageEncodingStatus  canSign canEncrypt securityError addressesFailingEncryption =
  withObjCPtr securityError $ \raw_securityError ->
    withObjCPtr addressesFailingEncryption $ \raw_addressesFailingEncryption ->
        sendMsg meOutgoingMessageEncodingStatus (mkSelector "initWithCanSign:canEncrypt:securityError:addressesFailingEncryption:") (retPtr retVoid) [argCULong (if canSign then 1 else 0), argCULong (if canEncrypt then 1 else 0), argPtr (castPtr raw_securityError :: Ptr ()), argPtr (castPtr raw_addressesFailingEncryption :: Ptr ())] >>= ownedObject . castPtr

-- | Whether or not the message can be signed.
--
-- ObjC selector: @- canSign@
canSign :: IsMEOutgoingMessageEncodingStatus meOutgoingMessageEncodingStatus => meOutgoingMessageEncodingStatus -> IO Bool
canSign meOutgoingMessageEncodingStatus  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg meOutgoingMessageEncodingStatus (mkSelector "canSign") retCULong []

-- | Whether or not the message can be encrypted.
--
-- ObjC selector: @- canEncrypt@
canEncrypt :: IsMEOutgoingMessageEncodingStatus meOutgoingMessageEncodingStatus => meOutgoingMessageEncodingStatus -> IO Bool
canEncrypt meOutgoingMessageEncodingStatus  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg meOutgoingMessageEncodingStatus (mkSelector "canEncrypt") retCULong []

-- | Any error that occurred while verifying the security status for the outgoing mail message.
--
-- ObjC selector: @- securityError@
securityError :: IsMEOutgoingMessageEncodingStatus meOutgoingMessageEncodingStatus => meOutgoingMessageEncodingStatus -> IO (Id NSError)
securityError meOutgoingMessageEncodingStatus  =
    sendMsg meOutgoingMessageEncodingStatus (mkSelector "securityError") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | A list of any recipients for which the message should be encrypted but an error occurred. This could include missing the public key for the recipient.
--
-- ObjC selector: @- addressesFailingEncryption@
addressesFailingEncryption :: IsMEOutgoingMessageEncodingStatus meOutgoingMessageEncodingStatus => meOutgoingMessageEncodingStatus -> IO (Id NSArray)
addressesFailingEncryption meOutgoingMessageEncodingStatus  =
    sendMsg meOutgoingMessageEncodingStatus (mkSelector "addressesFailingEncryption") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCanSign:canEncrypt:securityError:addressesFailingEncryption:@
initWithCanSign_canEncrypt_securityError_addressesFailingEncryptionSelector :: Selector
initWithCanSign_canEncrypt_securityError_addressesFailingEncryptionSelector = mkSelector "initWithCanSign:canEncrypt:securityError:addressesFailingEncryption:"

-- | @Selector@ for @canSign@
canSignSelector :: Selector
canSignSelector = mkSelector "canSign"

-- | @Selector@ for @canEncrypt@
canEncryptSelector :: Selector
canEncryptSelector = mkSelector "canEncrypt"

-- | @Selector@ for @securityError@
securityErrorSelector :: Selector
securityErrorSelector = mkSelector "securityError"

-- | @Selector@ for @addressesFailingEncryption@
addressesFailingEncryptionSelector :: Selector
addressesFailingEncryptionSelector = mkSelector "addressesFailingEncryption"

