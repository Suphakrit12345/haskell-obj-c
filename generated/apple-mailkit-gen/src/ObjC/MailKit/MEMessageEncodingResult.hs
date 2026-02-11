{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Contains information about an outging mail message after any security measures have been applied.
--
-- Generated bindings for @MEMessageEncodingResult@.
module ObjC.MailKit.MEMessageEncodingResult
  ( MEMessageEncodingResult
  , IsMEMessageEncodingResult(..)
  , new
  , init_
  , initWithEncodedMessage_signingError_encryptionError
  , encodedMessage
  , signingError
  , encryptionError
  , newSelector
  , initSelector
  , initWithEncodedMessage_signingError_encryptionErrorSelector
  , encodedMessageSelector
  , signingErrorSelector
  , encryptionErrorSelector


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
new :: IO (Id MEMessageEncodingResult)
new  =
  do
    cls' <- getRequiredClass "MEMessageEncodingResult"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsMEMessageEncodingResult meMessageEncodingResult => meMessageEncodingResult -> IO (Id MEMessageEncodingResult)
init_ meMessageEncodingResult  =
    sendMsg meMessageEncodingResult (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithEncodedMessage:signingError:encryptionError:@
initWithEncodedMessage_signingError_encryptionError :: (IsMEMessageEncodingResult meMessageEncodingResult, IsMEEncodedOutgoingMessage encodedMessage, IsNSError signingError, IsNSError encryptionError) => meMessageEncodingResult -> encodedMessage -> signingError -> encryptionError -> IO (Id MEMessageEncodingResult)
initWithEncodedMessage_signingError_encryptionError meMessageEncodingResult  encodedMessage signingError encryptionError =
  withObjCPtr encodedMessage $ \raw_encodedMessage ->
    withObjCPtr signingError $ \raw_signingError ->
      withObjCPtr encryptionError $ \raw_encryptionError ->
          sendMsg meMessageEncodingResult (mkSelector "initWithEncodedMessage:signingError:encryptionError:") (retPtr retVoid) [argPtr (castPtr raw_encodedMessage :: Ptr ()), argPtr (castPtr raw_signingError :: Ptr ()), argPtr (castPtr raw_encryptionError :: Ptr ())] >>= ownedObject . castPtr

-- | The encoded message. Nil if no need to encode or an error occured while encoding
--
-- ObjC selector: @- encodedMessage@
encodedMessage :: IsMEMessageEncodingResult meMessageEncodingResult => meMessageEncodingResult -> IO (Id MEEncodedOutgoingMessage)
encodedMessage meMessageEncodingResult  =
    sendMsg meMessageEncodingResult (mkSelector "encodedMessage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Any error that occured while attempting to sign the outgoing message.
--
-- ObjC selector: @- signingError@
signingError :: IsMEMessageEncodingResult meMessageEncodingResult => meMessageEncodingResult -> IO (Id NSError)
signingError meMessageEncodingResult  =
    sendMsg meMessageEncodingResult (mkSelector "signingError") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Any error that occured while attempting to encrypt the outgoing message.
--
-- ObjC selector: @- encryptionError@
encryptionError :: IsMEMessageEncodingResult meMessageEncodingResult => meMessageEncodingResult -> IO (Id NSError)
encryptionError meMessageEncodingResult  =
    sendMsg meMessageEncodingResult (mkSelector "encryptionError") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithEncodedMessage:signingError:encryptionError:@
initWithEncodedMessage_signingError_encryptionErrorSelector :: Selector
initWithEncodedMessage_signingError_encryptionErrorSelector = mkSelector "initWithEncodedMessage:signingError:encryptionError:"

-- | @Selector@ for @encodedMessage@
encodedMessageSelector :: Selector
encodedMessageSelector = mkSelector "encodedMessage"

-- | @Selector@ for @signingError@
signingErrorSelector :: Selector
signingErrorSelector = mkSelector "signingError"

-- | @Selector@ for @encryptionError@
encryptionErrorSelector :: Selector
encryptionErrorSelector = mkSelector "encryptionError"

