{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contains security information about a decoded message
--
-- Generated bindings for @MEMessageSecurityInformation@.
module ObjC.MailKit.MEMessageSecurityInformation
  ( MEMessageSecurityInformation
  , IsMEMessageSecurityInformation(..)
  , new
  , init_
  , initWithSigners_isEncrypted_signingError_encryptionError
  , initWithSigners_isEncrypted_signingError_encryptionError_shouldBlockRemoteContent_localizedRemoteContentBlockingReason
  , signers
  , isEncrypted
  , signingError
  , encryptionError
  , shouldBlockRemoteContent
  , localizedRemoteContentBlockingReason
  , newSelector
  , initSelector
  , initWithSigners_isEncrypted_signingError_encryptionErrorSelector
  , initWithSigners_isEncrypted_signingError_encryptionError_shouldBlockRemoteContent_localizedRemoteContentBlockingReasonSelector
  , signersSelector
  , isEncryptedSelector
  , signingErrorSelector
  , encryptionErrorSelector
  , shouldBlockRemoteContentSelector
  , localizedRemoteContentBlockingReasonSelector


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
new :: IO (Id MEMessageSecurityInformation)
new  =
  do
    cls' <- getRequiredClass "MEMessageSecurityInformation"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMEMessageSecurityInformation meMessageSecurityInformation => meMessageSecurityInformation -> IO (Id MEMessageSecurityInformation)
init_ meMessageSecurityInformation  =
    sendMsg meMessageSecurityInformation (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithSigners:isEncrypted:signingError:encryptionError:@
initWithSigners_isEncrypted_signingError_encryptionError :: (IsMEMessageSecurityInformation meMessageSecurityInformation, IsNSArray signers, IsNSError signingError, IsNSError encryptionError) => meMessageSecurityInformation -> signers -> Bool -> signingError -> encryptionError -> IO (Id MEMessageSecurityInformation)
initWithSigners_isEncrypted_signingError_encryptionError meMessageSecurityInformation  signers isEncrypted signingError encryptionError =
  withObjCPtr signers $ \raw_signers ->
    withObjCPtr signingError $ \raw_signingError ->
      withObjCPtr encryptionError $ \raw_encryptionError ->
          sendMsg meMessageSecurityInformation (mkSelector "initWithSigners:isEncrypted:signingError:encryptionError:") (retPtr retVoid) [argPtr (castPtr raw_signers :: Ptr ()), argCULong (if isEncrypted then 1 else 0), argPtr (castPtr raw_signingError :: Ptr ()), argPtr (castPtr raw_encryptionError :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithSigners:isEncrypted:signingError:encryptionError:shouldBlockRemoteContent:localizedRemoteContentBlockingReason:@
initWithSigners_isEncrypted_signingError_encryptionError_shouldBlockRemoteContent_localizedRemoteContentBlockingReason :: (IsMEMessageSecurityInformation meMessageSecurityInformation, IsNSArray signers, IsNSError signingError, IsNSError encryptionError, IsNSString localizedRemoteContentBlockingReason) => meMessageSecurityInformation -> signers -> Bool -> signingError -> encryptionError -> Bool -> localizedRemoteContentBlockingReason -> IO (Id MEMessageSecurityInformation)
initWithSigners_isEncrypted_signingError_encryptionError_shouldBlockRemoteContent_localizedRemoteContentBlockingReason meMessageSecurityInformation  signers isEncrypted signingError encryptionError shouldBlockRemoteContent localizedRemoteContentBlockingReason =
  withObjCPtr signers $ \raw_signers ->
    withObjCPtr signingError $ \raw_signingError ->
      withObjCPtr encryptionError $ \raw_encryptionError ->
        withObjCPtr localizedRemoteContentBlockingReason $ \raw_localizedRemoteContentBlockingReason ->
            sendMsg meMessageSecurityInformation (mkSelector "initWithSigners:isEncrypted:signingError:encryptionError:shouldBlockRemoteContent:localizedRemoteContentBlockingReason:") (retPtr retVoid) [argPtr (castPtr raw_signers :: Ptr ()), argCULong (if isEncrypted then 1 else 0), argPtr (castPtr raw_signingError :: Ptr ()), argPtr (castPtr raw_encryptionError :: Ptr ()), argCULong (if shouldBlockRemoteContent then 1 else 0), argPtr (castPtr raw_localizedRemoteContentBlockingReason :: Ptr ())] >>= ownedObject . castPtr

-- | The signers of the message
--
-- ObjC selector: @- signers@
signers :: IsMEMessageSecurityInformation meMessageSecurityInformation => meMessageSecurityInformation -> IO (Id NSArray)
signers meMessageSecurityInformation  =
    sendMsg meMessageSecurityInformation (mkSelector "signers") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether or not the message was encrypted.
--
-- ObjC selector: @- isEncrypted@
isEncrypted :: IsMEMessageSecurityInformation meMessageSecurityInformation => meMessageSecurityInformation -> IO Bool
isEncrypted meMessageSecurityInformation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg meMessageSecurityInformation (mkSelector "isEncrypted") retCULong []

-- | Any signing error that occured when decoding the message.
--
-- ObjC selector: @- signingError@
signingError :: IsMEMessageSecurityInformation meMessageSecurityInformation => meMessageSecurityInformation -> IO (Id NSError)
signingError meMessageSecurityInformation  =
    sendMsg meMessageSecurityInformation (mkSelector "signingError") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Any encryption error that occured when decoding the message.
--
-- ObjC selector: @- encryptionError@
encryptionError :: IsMEMessageSecurityInformation meMessageSecurityInformation => meMessageSecurityInformation -> IO (Id NSError)
encryptionError meMessageSecurityInformation  =
    sendMsg meMessageSecurityInformation (mkSelector "encryptionError") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether or not Mail should block loading remote content for the message by default. The user will have the option to load remote content manually.
--
-- ObjC selector: @- shouldBlockRemoteContent@
shouldBlockRemoteContent :: IsMEMessageSecurityInformation meMessageSecurityInformation => meMessageSecurityInformation -> IO Bool
shouldBlockRemoteContent meMessageSecurityInformation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg meMessageSecurityInformation (mkSelector "shouldBlockRemoteContent") retCULong []

-- | A localized string containing the reason for blocking remote content.
--
-- ObjC selector: @- localizedRemoteContentBlockingReason@
localizedRemoteContentBlockingReason :: IsMEMessageSecurityInformation meMessageSecurityInformation => meMessageSecurityInformation -> IO (Id NSString)
localizedRemoteContentBlockingReason meMessageSecurityInformation  =
    sendMsg meMessageSecurityInformation (mkSelector "localizedRemoteContentBlockingReason") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithSigners:isEncrypted:signingError:encryptionError:@
initWithSigners_isEncrypted_signingError_encryptionErrorSelector :: Selector
initWithSigners_isEncrypted_signingError_encryptionErrorSelector = mkSelector "initWithSigners:isEncrypted:signingError:encryptionError:"

-- | @Selector@ for @initWithSigners:isEncrypted:signingError:encryptionError:shouldBlockRemoteContent:localizedRemoteContentBlockingReason:@
initWithSigners_isEncrypted_signingError_encryptionError_shouldBlockRemoteContent_localizedRemoteContentBlockingReasonSelector :: Selector
initWithSigners_isEncrypted_signingError_encryptionError_shouldBlockRemoteContent_localizedRemoteContentBlockingReasonSelector = mkSelector "initWithSigners:isEncrypted:signingError:encryptionError:shouldBlockRemoteContent:localizedRemoteContentBlockingReason:"

-- | @Selector@ for @signers@
signersSelector :: Selector
signersSelector = mkSelector "signers"

-- | @Selector@ for @isEncrypted@
isEncryptedSelector :: Selector
isEncryptedSelector = mkSelector "isEncrypted"

-- | @Selector@ for @signingError@
signingErrorSelector :: Selector
signingErrorSelector = mkSelector "signingError"

-- | @Selector@ for @encryptionError@
encryptionErrorSelector :: Selector
encryptionErrorSelector = mkSelector "encryptionError"

-- | @Selector@ for @shouldBlockRemoteContent@
shouldBlockRemoteContentSelector :: Selector
shouldBlockRemoteContentSelector = mkSelector "shouldBlockRemoteContent"

-- | @Selector@ for @localizedRemoteContentBlockingReason@
localizedRemoteContentBlockingReasonSelector :: Selector
localizedRemoteContentBlockingReasonSelector = mkSelector "localizedRemoteContentBlockingReason"

