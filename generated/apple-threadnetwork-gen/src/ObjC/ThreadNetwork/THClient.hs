{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class that supports safely sharing Thread credentials between multiple clients.
--
-- Request credentials for either a specific Thread network or for the _preferred network_ using @THClient@. The preferred network is the default Thread network chosen by the framework for a home.
--
-- The ThreadNetwork framework maintains a database of network credentials. The class allows clients to store, list, and delete credentials for a given network from the database.
--
-- Some methods in @THClient@ use the _team ID_, a string that you store in your application’s @Info.plist@. The ThreadNetwork framework uses the team ID to preserve the privacy of the Thread network credentials across different clients. For example, credentials stored by one client can’t be deleted or modified by another client.
--
-- - Important: Thread credentials give you the ability to add any device into   the Thread network. Use this information responsibly.
--
-- Generated bindings for @THClient@.
module ObjC.ThreadNetwork.THClient
  ( THClient
  , IsTHClient(..)
  , init_
  , deleteCredentialsForBorderAgent_completion
  , retrieveCredentialsForBorderAgent_completion
  , storeCredentialsForBorderAgent_activeOperationalDataSet_completion
  , retrievePreferredCredentials
  , retrieveCredentialsForExtendedPANID_completion
  , checkPreferredNetworkForActiveOperationalDataset_completion
  , isPreferredNetworkAvailableWithCompletion
  , initSelector
  , deleteCredentialsForBorderAgent_completionSelector
  , retrieveCredentialsForBorderAgent_completionSelector
  , storeCredentialsForBorderAgent_activeOperationalDataSet_completionSelector
  , retrievePreferredCredentialsSelector
  , retrieveCredentialsForExtendedPANID_completionSelector
  , checkPreferredNetworkForActiveOperationalDataset_completionSelector
  , isPreferredNetworkAvailableWithCompletionSelector


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

import ObjC.ThreadNetwork.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates the client object.
--
-- - Returns: An instance of the client object.
--
-- ObjC selector: @- init@
init_ :: IsTHClient thClient => thClient -> IO (Id THClient)
init_ thClient  =
    sendMsg thClient (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | Deletes Thread network credentials from the framework database for a Border Agent.
--
-- The Border Agent is the software component running in the Border Router responsible for advertising itself in the Wi-Fi or Ethernet network.
--
-- - Parameters:   - borderAgentID: The identifer of a Thread network Border Agent.   - completion: The completion handler the framework calls after deleting     the credentials.
--
-- > Concurrency Note: You can call this method from synchronous code using a completion handler, > as shown on this page, or you can call it as an asynchronous method that has the > following declaration: > > ```swift > func deleteCredentials(forBorderAgent borderAgentID: Data) async throws > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- ObjC selector: @- deleteCredentialsForBorderAgent:completion:@
deleteCredentialsForBorderAgent_completion :: (IsTHClient thClient, IsNSData borderAgentID) => thClient -> borderAgentID -> Ptr () -> IO ()
deleteCredentialsForBorderAgent_completion thClient  borderAgentID completion =
  withObjCPtr borderAgentID $ \raw_borderAgentID ->
      sendMsg thClient (mkSelector "deleteCredentialsForBorderAgent:completion:") retVoid [argPtr (castPtr raw_borderAgentID :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Requests Thread credentials for a Border Agent.
--
-- The framework identifies the developer by the team ID. When calling this method, you receive credentials for your team ID only.
--
-- - Parameters:   - borderAgentID: The identifer of a Thread network Border Agent.   - completion: The completion handler the framework calls when the     credentials become available.
--
-- > Concurrency Note: You can call this method from synchronous code using a completion handler, > as shown on this page, or you can call it as an asynchronous method that has the > following declaration: > > ```swift > func credentials(forBorderAgentID borderAgentID: Data) async throws -> THCredentials > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- ObjC selector: @- retrieveCredentialsForBorderAgent:completion:@
retrieveCredentialsForBorderAgent_completion :: (IsTHClient thClient, IsNSData borderAgentID) => thClient -> borderAgentID -> Ptr () -> IO ()
retrieveCredentialsForBorderAgent_completion thClient  borderAgentID completion =
  withObjCPtr borderAgentID $ \raw_borderAgentID ->
      sendMsg thClient (mkSelector "retrieveCredentialsForBorderAgent:completion:") retVoid [argPtr (castPtr raw_borderAgentID :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Stores Thread network credentials into the framework database that a Border Agent provides.
--
-- The Border Agent is the software component running in the Border Router responsible for advertising itself in the Wi-Fi or Ethernet network.
--
-- The framework only stores credentials if it can find an mDNS record for the Border Agent that contains the specified Border Agent identifier.
--
-- - Parameters:   - borderAgentID: The identifer of an active Thread network Border Agent.   - activeOperationalDataSet: The essential operational parameters for the     Thread network.   - completion: The completion handler the framework calls after storing the credentials.
--
-- > Concurrency Note: You can call this method from synchronous code using a completion handler, > as shown on this page, or you can call it as an asynchronous method that has the > following declaration: > > ```swift > func storeCredentials(forBorderAgent borderAgentID: Data, activeOperationalDataSet: Data) async throws > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- ObjC selector: @- storeCredentialsForBorderAgent:activeOperationalDataSet:completion:@
storeCredentialsForBorderAgent_activeOperationalDataSet_completion :: (IsTHClient thClient, IsNSData borderAgentID, IsNSData activeOperationalDataSet) => thClient -> borderAgentID -> activeOperationalDataSet -> Ptr () -> IO ()
storeCredentialsForBorderAgent_activeOperationalDataSet_completion thClient  borderAgentID activeOperationalDataSet completion =
  withObjCPtr borderAgentID $ \raw_borderAgentID ->
    withObjCPtr activeOperationalDataSet $ \raw_activeOperationalDataSet ->
        sendMsg thClient (mkSelector "storeCredentialsForBorderAgent:activeOperationalDataSet:completion:") retVoid [argPtr (castPtr raw_borderAgentID :: Ptr ()), argPtr (castPtr raw_activeOperationalDataSet :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Requests Thread credentials for the preferred network.
--
-- When you call this method, an alert appears asking for user permission to access credentials.
--
-- - Parameters:   - completion: The completion handler the framework calls when the     credentials become available.
--
-- > Concurrency Note: You can call this method from synchronous code using a completion handler, > as shown on this page, or you can call it as an asynchronous method that has the > following declaration: > > ```swift > func preferredCredentials() async throws -> THCredentials > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- ObjC selector: @- retrievePreferredCredentials:@
retrievePreferredCredentials :: IsTHClient thClient => thClient -> Ptr () -> IO ()
retrievePreferredCredentials thClient  completion =
    sendMsg thClient (mkSelector "retrievePreferredCredentials:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- | Requests Thread credentials for an extended Personal Area Network (PAN) ID.
--
-- When calling this method, an alert appears asking for user permission to access credentials.
--
-- - Parameters:   - extendedPANID: The extended PAN identifier.   - completion: The completion handler the framework calls when the     credentials become available.
--
-- > Concurrency Note: You can call this method from synchronous code using a completion handler, > as shown on this page, or you can call it as an asynchronous method that has the > following declaration: > > ```swift > func credentials(forExtendedPANID extendedPANID: Data) async throws -> THCredentials > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- ObjC selector: @- retrieveCredentialsForExtendedPANID:completion:@
retrieveCredentialsForExtendedPANID_completion :: (IsTHClient thClient, IsNSData extendedPANID) => thClient -> extendedPANID -> Ptr () -> IO ()
retrieveCredentialsForExtendedPANID_completion thClient  extendedPANID completion =
  withObjCPtr extendedPANID $ \raw_extendedPANID ->
      sendMsg thClient (mkSelector "retrieveCredentialsForExtendedPANID:completion:") retVoid [argPtr (castPtr raw_extendedPANID :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Determines if the essential operating parameters match the preferred network’s parameters.
--
-- - Parameters:   - activeOperationalDataSet: The essential operating parameters to compare     against the preferred network’s parameters.   - completion: The completion handler that returns the result of the     comparison.
--
-- > Concurrency Note: You can call this method from synchronous code using a completion handler, > as shown on this page, or you can call it as an asynchronous method that has the > following declaration: > > ```swift > func isPreferred(forActiveOperationalDataset activeOperationalDataSet: Data) async -> Bool > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- ObjC selector: @- checkPreferredNetworkForActiveOperationalDataset:completion:@
checkPreferredNetworkForActiveOperationalDataset_completion :: (IsTHClient thClient, IsNSData activeOperationalDataSet) => thClient -> activeOperationalDataSet -> Ptr () -> IO ()
checkPreferredNetworkForActiveOperationalDataset_completion thClient  activeOperationalDataSet completion =
  withObjCPtr activeOperationalDataSet $ \raw_activeOperationalDataSet ->
      sendMsg thClient (mkSelector "checkPreferredNetworkForActiveOperationalDataset:completion:") retVoid [argPtr (castPtr raw_activeOperationalDataSet :: Ptr ()), argPtr (castPtr completion :: Ptr ())]

-- | Determines if the preferred network is available or not
--
-- - Parameters:   - completion: The completion handler that returns the result of the     preferred network availability.
--
-- > Concurrency Note: You can call this method from synchronous code using a completion handler, > as shown on this page, or you can call it as an asynchronous method that has the > following declaration: > > ```swift > func isPreferredAvailable() async -> Bool > ``` > > For information about concurrency and asynchronous code in Swift, see <doc://com.apple.documentation/documentation/swift/calling-objective-c-apis-asynchronously>.
--
-- ObjC selector: @- isPreferredNetworkAvailableWithCompletion:@
isPreferredNetworkAvailableWithCompletion :: IsTHClient thClient => thClient -> Ptr () -> IO ()
isPreferredNetworkAvailableWithCompletion thClient  completion =
    sendMsg thClient (mkSelector "isPreferredNetworkAvailableWithCompletion:") retVoid [argPtr (castPtr completion :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @deleteCredentialsForBorderAgent:completion:@
deleteCredentialsForBorderAgent_completionSelector :: Selector
deleteCredentialsForBorderAgent_completionSelector = mkSelector "deleteCredentialsForBorderAgent:completion:"

-- | @Selector@ for @retrieveCredentialsForBorderAgent:completion:@
retrieveCredentialsForBorderAgent_completionSelector :: Selector
retrieveCredentialsForBorderAgent_completionSelector = mkSelector "retrieveCredentialsForBorderAgent:completion:"

-- | @Selector@ for @storeCredentialsForBorderAgent:activeOperationalDataSet:completion:@
storeCredentialsForBorderAgent_activeOperationalDataSet_completionSelector :: Selector
storeCredentialsForBorderAgent_activeOperationalDataSet_completionSelector = mkSelector "storeCredentialsForBorderAgent:activeOperationalDataSet:completion:"

-- | @Selector@ for @retrievePreferredCredentials:@
retrievePreferredCredentialsSelector :: Selector
retrievePreferredCredentialsSelector = mkSelector "retrievePreferredCredentials:"

-- | @Selector@ for @retrieveCredentialsForExtendedPANID:completion:@
retrieveCredentialsForExtendedPANID_completionSelector :: Selector
retrieveCredentialsForExtendedPANID_completionSelector = mkSelector "retrieveCredentialsForExtendedPANID:completion:"

-- | @Selector@ for @checkPreferredNetworkForActiveOperationalDataset:completion:@
checkPreferredNetworkForActiveOperationalDataset_completionSelector :: Selector
checkPreferredNetworkForActiveOperationalDataset_completionSelector = mkSelector "checkPreferredNetworkForActiveOperationalDataset:completion:"

-- | @Selector@ for @isPreferredNetworkAvailableWithCompletion:@
isPreferredNetworkAvailableWithCompletionSelector :: Selector
isPreferredNetworkAvailableWithCompletionSelector = mkSelector "isPreferredNetworkAvailableWithCompletion:"

