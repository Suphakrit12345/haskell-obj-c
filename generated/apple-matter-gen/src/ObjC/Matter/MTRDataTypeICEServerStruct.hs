{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MTRDataTypeICEServerStruct@.
module ObjC.Matter.MTRDataTypeICEServerStruct
  ( MTRDataTypeICEServerStruct
  , IsMTRDataTypeICEServerStruct(..)
  , urls
  , setUrls
  , username
  , setUsername
  , credential
  , setCredential
  , caid
  , setCaid
  , urlsSelector
  , setUrlsSelector
  , usernameSelector
  , setUsernameSelector
  , credentialSelector
  , setCredentialSelector
  , caidSelector
  , setCaidSelector


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

import ObjC.Matter.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- urls@
urls :: IsMTRDataTypeICEServerStruct mtrDataTypeICEServerStruct => mtrDataTypeICEServerStruct -> IO (Id NSArray)
urls mtrDataTypeICEServerStruct  =
    sendMsg mtrDataTypeICEServerStruct (mkSelector "urls") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUrls:@
setUrls :: (IsMTRDataTypeICEServerStruct mtrDataTypeICEServerStruct, IsNSArray value) => mtrDataTypeICEServerStruct -> value -> IO ()
setUrls mtrDataTypeICEServerStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeICEServerStruct (mkSelector "setUrls:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- username@
username :: IsMTRDataTypeICEServerStruct mtrDataTypeICEServerStruct => mtrDataTypeICEServerStruct -> IO (Id NSString)
username mtrDataTypeICEServerStruct  =
    sendMsg mtrDataTypeICEServerStruct (mkSelector "username") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setUsername:@
setUsername :: (IsMTRDataTypeICEServerStruct mtrDataTypeICEServerStruct, IsNSString value) => mtrDataTypeICEServerStruct -> value -> IO ()
setUsername mtrDataTypeICEServerStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeICEServerStruct (mkSelector "setUsername:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- credential@
credential :: IsMTRDataTypeICEServerStruct mtrDataTypeICEServerStruct => mtrDataTypeICEServerStruct -> IO (Id NSString)
credential mtrDataTypeICEServerStruct  =
    sendMsg mtrDataTypeICEServerStruct (mkSelector "credential") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCredential:@
setCredential :: (IsMTRDataTypeICEServerStruct mtrDataTypeICEServerStruct, IsNSString value) => mtrDataTypeICEServerStruct -> value -> IO ()
setCredential mtrDataTypeICEServerStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeICEServerStruct (mkSelector "setCredential:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- caid@
caid :: IsMTRDataTypeICEServerStruct mtrDataTypeICEServerStruct => mtrDataTypeICEServerStruct -> IO (Id NSNumber)
caid mtrDataTypeICEServerStruct  =
    sendMsg mtrDataTypeICEServerStruct (mkSelector "caid") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setCaid:@
setCaid :: (IsMTRDataTypeICEServerStruct mtrDataTypeICEServerStruct, IsNSNumber value) => mtrDataTypeICEServerStruct -> value -> IO ()
setCaid mtrDataTypeICEServerStruct  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtrDataTypeICEServerStruct (mkSelector "setCaid:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @urls@
urlsSelector :: Selector
urlsSelector = mkSelector "urls"

-- | @Selector@ for @setUrls:@
setUrlsSelector :: Selector
setUrlsSelector = mkSelector "setUrls:"

-- | @Selector@ for @username@
usernameSelector :: Selector
usernameSelector = mkSelector "username"

-- | @Selector@ for @setUsername:@
setUsernameSelector :: Selector
setUsernameSelector = mkSelector "setUsername:"

-- | @Selector@ for @credential@
credentialSelector :: Selector
credentialSelector = mkSelector "credential"

-- | @Selector@ for @setCredential:@
setCredentialSelector :: Selector
setCredentialSelector = mkSelector "setCredential:"

-- | @Selector@ for @caid@
caidSelector :: Selector
caidSelector = mkSelector "caid"

-- | @Selector@ for @setCaid:@
setCaidSelector :: Selector
setCaidSelector = mkSelector "setCaid:"

