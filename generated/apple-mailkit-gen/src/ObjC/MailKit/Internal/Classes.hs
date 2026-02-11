{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Internal module: all type tags, aliases, type classes, and
-- hierarchy instances for this framework.
--
-- Exists to break import cycles between per-class modules.
-- Import the per-class modules for the public API.
module ObjC.MailKit.Internal.Classes (
    module ObjC.MailKit.Internal.Classes,
    module ObjC.AppKit.Internal.Classes,
    module ObjC.Foundation.Internal.Classes,
  ) where

import Data.Proxy (Proxy(..))
import ObjC.Runtime.Types
import ObjC.Runtime.Class (getRequiredClass)
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- ---------- MEAddressAnnotation ----------

-- | An instance of this class can be used to change the visual style of recipeint email address token when user in composing a mail message.
-- 
-- Phantom type for @MEAddressAnnotation@.
data MEAddressAnnotation

instance IsObjCObject (Id MEAddressAnnotation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEAddressAnnotation"

class IsNSObject a => IsMEAddressAnnotation a where
  toMEAddressAnnotation :: a -> Id MEAddressAnnotation

instance IsMEAddressAnnotation (Id MEAddressAnnotation) where
  toMEAddressAnnotation = unsafeCastId

instance IsNSObject (Id MEAddressAnnotation) where
  toNSObject = unsafeCastId

-- ---------- MEComposeContext ----------

-- | An object encapsulating additional information about the message being composed.
-- 
-- Phantom type for @MEComposeContext@.
data MEComposeContext

instance IsObjCObject (Id MEComposeContext) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEComposeContext"

class IsNSObject a => IsMEComposeContext a where
  toMEComposeContext :: a -> Id MEComposeContext

instance IsMEComposeContext (Id MEComposeContext) where
  toMEComposeContext = unsafeCastId

instance IsNSObject (Id MEComposeContext) where
  toNSObject = unsafeCastId

-- ---------- MEComposeSession ----------

-- | An instance of this class is associated with the lifecycle of a single mail compose window. This object associates the actions performed by the user in a mail compose window to a unique session. An instance of this class is passed to the methods in @MEComposeSessionHandler.@
-- 
-- Phantom type for @MEComposeSession@.
data MEComposeSession

instance IsObjCObject (Id MEComposeSession) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEComposeSession"

class IsNSObject a => IsMEComposeSession a where
  toMEComposeSession :: a -> Id MEComposeSession

instance IsMEComposeSession (Id MEComposeSession) where
  toMEComposeSession = unsafeCastId

instance IsNSObject (Id MEComposeSession) where
  toNSObject = unsafeCastId

-- ---------- MEDecodedMessage ----------

-- | Contains information about a decoded message
-- 
-- Phantom type for @MEDecodedMessage@.
data MEDecodedMessage

instance IsObjCObject (Id MEDecodedMessage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEDecodedMessage"

class IsNSObject a => IsMEDecodedMessage a where
  toMEDecodedMessage :: a -> Id MEDecodedMessage

instance IsMEDecodedMessage (Id MEDecodedMessage) where
  toMEDecodedMessage = unsafeCastId

instance IsNSObject (Id MEDecodedMessage) where
  toNSObject = unsafeCastId

-- ---------- MEDecodedMessageBanner ----------

-- | Contains security information in order to populate a banner in the message view.
-- 
-- Phantom type for @MEDecodedMessageBanner@.
data MEDecodedMessageBanner

instance IsObjCObject (Id MEDecodedMessageBanner) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEDecodedMessageBanner"

class IsNSObject a => IsMEDecodedMessageBanner a where
  toMEDecodedMessageBanner :: a -> Id MEDecodedMessageBanner

instance IsMEDecodedMessageBanner (Id MEDecodedMessageBanner) where
  toMEDecodedMessageBanner = unsafeCastId

instance IsNSObject (Id MEDecodedMessageBanner) where
  toNSObject = unsafeCastId

-- ---------- MEEmailAddress ----------

-- | Contain information about an email address. This can include both valid and invalid email addresses.
-- 
-- Phantom type for @MEEmailAddress@.
data MEEmailAddress

instance IsObjCObject (Id MEEmailAddress) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEEmailAddress"

class IsNSObject a => IsMEEmailAddress a where
  toMEEmailAddress :: a -> Id MEEmailAddress

instance IsMEEmailAddress (Id MEEmailAddress) where
  toMEEmailAddress = unsafeCastId

instance IsNSObject (Id MEEmailAddress) where
  toNSObject = unsafeCastId

-- ---------- MEEncodedOutgoingMessage ----------

-- | Phantom type for @MEEncodedOutgoingMessage@.
data MEEncodedOutgoingMessage

instance IsObjCObject (Id MEEncodedOutgoingMessage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEEncodedOutgoingMessage"

class IsNSObject a => IsMEEncodedOutgoingMessage a where
  toMEEncodedOutgoingMessage :: a -> Id MEEncodedOutgoingMessage

instance IsMEEncodedOutgoingMessage (Id MEEncodedOutgoingMessage) where
  toMEEncodedOutgoingMessage = unsafeCastId

instance IsNSObject (Id MEEncodedOutgoingMessage) where
  toNSObject = unsafeCastId

-- ---------- MEExtensionManager ----------

-- | Methods in this class allow the host app to interact with their Mail extension.
-- 
-- Phantom type for @MEExtensionManager@.
data MEExtensionManager

instance IsObjCObject (Id MEExtensionManager) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEExtensionManager"

class IsNSObject a => IsMEExtensionManager a where
  toMEExtensionManager :: a -> Id MEExtensionManager

instance IsMEExtensionManager (Id MEExtensionManager) where
  toMEExtensionManager = unsafeCastId

instance IsNSObject (Id MEExtensionManager) where
  toNSObject = unsafeCastId

-- ---------- MEMessage ----------

-- | Contains information about a mail message on which actions can be performed.
-- 
-- Phantom type for @MEMessage@.
data MEMessage

instance IsObjCObject (Id MEMessage) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEMessage"

class IsNSObject a => IsMEMessage a where
  toMEMessage :: a -> Id MEMessage

instance IsMEMessage (Id MEMessage) where
  toMEMessage = unsafeCastId

instance IsNSObject (Id MEMessage) where
  toNSObject = unsafeCastId

-- ---------- MEMessageAction ----------

-- | An action that can be performed on a mail message.
-- 
-- Phantom type for @MEMessageAction@.
data MEMessageAction

instance IsObjCObject (Id MEMessageAction) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEMessageAction"

class IsNSObject a => IsMEMessageAction a where
  toMEMessageAction :: a -> Id MEMessageAction

instance IsMEMessageAction (Id MEMessageAction) where
  toMEMessageAction = unsafeCastId

instance IsNSObject (Id MEMessageAction) where
  toNSObject = unsafeCastId

-- ---------- MEMessageActionDecision ----------

-- | Phantom type for @MEMessageActionDecision@.
data MEMessageActionDecision

instance IsObjCObject (Id MEMessageActionDecision) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEMessageActionDecision"

class IsNSObject a => IsMEMessageActionDecision a where
  toMEMessageActionDecision :: a -> Id MEMessageActionDecision

instance IsMEMessageActionDecision (Id MEMessageActionDecision) where
  toMEMessageActionDecision = unsafeCastId

instance IsNSObject (Id MEMessageActionDecision) where
  toNSObject = unsafeCastId

-- ---------- MEMessageEncodingResult ----------

-- | Contains information about an outging mail message after any security measures have been applied.
-- 
-- Phantom type for @MEMessageEncodingResult@.
data MEMessageEncodingResult

instance IsObjCObject (Id MEMessageEncodingResult) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEMessageEncodingResult"

class IsNSObject a => IsMEMessageEncodingResult a where
  toMEMessageEncodingResult :: a -> Id MEMessageEncodingResult

instance IsMEMessageEncodingResult (Id MEMessageEncodingResult) where
  toMEMessageEncodingResult = unsafeCastId

instance IsNSObject (Id MEMessageEncodingResult) where
  toNSObject = unsafeCastId

-- ---------- MEMessageSecurityInformation ----------

-- | Contains security information about a decoded message
-- 
-- Phantom type for @MEMessageSecurityInformation@.
data MEMessageSecurityInformation

instance IsObjCObject (Id MEMessageSecurityInformation) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEMessageSecurityInformation"

class IsNSObject a => IsMEMessageSecurityInformation a where
  toMEMessageSecurityInformation :: a -> Id MEMessageSecurityInformation

instance IsMEMessageSecurityInformation (Id MEMessageSecurityInformation) where
  toMEMessageSecurityInformation = unsafeCastId

instance IsNSObject (Id MEMessageSecurityInformation) where
  toNSObject = unsafeCastId

-- ---------- MEMessageSigner ----------

-- | Contains information about a message signer
-- 
-- Phantom type for @MEMessageSigner@.
data MEMessageSigner

instance IsObjCObject (Id MEMessageSigner) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEMessageSigner"

class IsNSObject a => IsMEMessageSigner a where
  toMEMessageSigner :: a -> Id MEMessageSigner

instance IsMEMessageSigner (Id MEMessageSigner) where
  toMEMessageSigner = unsafeCastId

instance IsNSObject (Id MEMessageSigner) where
  toNSObject = unsafeCastId

-- ---------- MEOutgoingMessageEncodingStatus ----------

-- | Contains information about any security measures that will be applied to a mail message when it is sent or any errrors that occurred while verifying security status.
-- 
-- Phantom type for @MEOutgoingMessageEncodingStatus@.
data MEOutgoingMessageEncodingStatus

instance IsObjCObject (Id MEOutgoingMessageEncodingStatus) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEOutgoingMessageEncodingStatus"

class IsNSObject a => IsMEOutgoingMessageEncodingStatus a where
  toMEOutgoingMessageEncodingStatus :: a -> Id MEOutgoingMessageEncodingStatus

instance IsMEOutgoingMessageEncodingStatus (Id MEOutgoingMessageEncodingStatus) where
  toMEOutgoingMessageEncodingStatus = unsafeCastId

instance IsNSObject (Id MEOutgoingMessageEncodingStatus) where
  toNSObject = unsafeCastId

-- ---------- MEExtensionViewController ----------

-- | Phantom type for @MEExtensionViewController@.
data MEExtensionViewController

instance IsObjCObject (Id MEExtensionViewController) where
  toRawId = idToRawId
  withObjCPtr = withIdObjCPtr
  staticClass _ = getRequiredClass "MEExtensionViewController"

class IsNSViewController a => IsMEExtensionViewController a where
  toMEExtensionViewController :: a -> Id MEExtensionViewController

instance IsMEExtensionViewController (Id MEExtensionViewController) where
  toMEExtensionViewController = unsafeCastId

instance IsNSObject (Id MEExtensionViewController) where
  toNSObject = unsafeCastId

instance IsNSResponder (Id MEExtensionViewController) where
  toNSResponder = unsafeCastId

instance IsNSViewController (Id MEExtensionViewController) where
  toNSViewController = unsafeCastId
