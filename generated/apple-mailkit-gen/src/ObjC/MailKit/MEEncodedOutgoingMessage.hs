{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MEEncodedOutgoingMessage@.
module ObjC.MailKit.MEEncodedOutgoingMessage
  ( MEEncodedOutgoingMessage
  , IsMEEncodedOutgoingMessage(..)
  , initWithRawData_isSigned_isEncrypted
  , rawData
  , isSigned
  , isEncrypted
  , initWithRawData_isSigned_isEncryptedSelector
  , rawDataSelector
  , isSignedSelector
  , isEncryptedSelector


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

-- | @- initWithRawData:isSigned:isEncrypted:@
initWithRawData_isSigned_isEncrypted :: (IsMEEncodedOutgoingMessage meEncodedOutgoingMessage, IsNSData rawData) => meEncodedOutgoingMessage -> rawData -> Bool -> Bool -> IO (Id MEEncodedOutgoingMessage)
initWithRawData_isSigned_isEncrypted meEncodedOutgoingMessage  rawData isSigned isEncrypted =
  withObjCPtr rawData $ \raw_rawData ->
      sendMsg meEncodedOutgoingMessage (mkSelector "initWithRawData:isSigned:isEncrypted:") (retPtr retVoid) [argPtr (castPtr raw_rawData :: Ptr ()), argCULong (if isSigned then 1 else 0), argCULong (if isEncrypted then 1 else 0)] >>= ownedObject . castPtr

-- | The full encoded RFC822 message including headers and body.
--
-- ObjC selector: @- rawData@
rawData :: IsMEEncodedOutgoingMessage meEncodedOutgoingMessage => meEncodedOutgoingMessage -> IO (Id NSData)
rawData meEncodedOutgoingMessage  =
    sendMsg meEncodedOutgoingMessage (mkSelector "rawData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Whether or not the encoded message is signed
--
-- ObjC selector: @- isSigned@
isSigned :: IsMEEncodedOutgoingMessage meEncodedOutgoingMessage => meEncodedOutgoingMessage -> IO Bool
isSigned meEncodedOutgoingMessage  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg meEncodedOutgoingMessage (mkSelector "isSigned") retCULong []

-- | Whether or not the encoded message is encrypted
--
-- ObjC selector: @- isEncrypted@
isEncrypted :: IsMEEncodedOutgoingMessage meEncodedOutgoingMessage => meEncodedOutgoingMessage -> IO Bool
isEncrypted meEncodedOutgoingMessage  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg meEncodedOutgoingMessage (mkSelector "isEncrypted") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithRawData:isSigned:isEncrypted:@
initWithRawData_isSigned_isEncryptedSelector :: Selector
initWithRawData_isSigned_isEncryptedSelector = mkSelector "initWithRawData:isSigned:isEncrypted:"

-- | @Selector@ for @rawData@
rawDataSelector :: Selector
rawDataSelector = mkSelector "rawData"

-- | @Selector@ for @isSigned@
isSignedSelector :: Selector
isSignedSelector = mkSelector "isSigned"

-- | @Selector@ for @isEncrypted@
isEncryptedSelector :: Selector
isEncryptedSelector = mkSelector "isEncrypted"

