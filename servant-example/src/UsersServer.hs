{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}

module UsersServer where

import Control.Monad.Trans.Except (throwE)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as M
import Data.List
import Data.Proxy
import Data.Text.Lazy hiding (filter)
import Data.Text.Lazy.Encoding
import Data.Text hiding (filter)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Docs
import Servant.Docs.Internal (ToAuthInfo(..), DocAuthentication(..))
import Servant.Server.Experimental.Auth
import Types

user1 :: User
user1 = User "John Doe" "john.doe@gmail.com" 24

user2 :: User
user2 = User "Jane Doe" "jane.doe@gmail.com" 25

userMap :: M.Map Int User
userMap = M.fromList [(1, user1), (2,user2)]

allUsers :: [User]
allUsers = M.elems userMap

type instance AuthServerData (AuthProtect "admin") = Int

type UsersAPI = "api" :> "users" :> 
  (BasicAuth "admin" User :> Get '[JSON] [User]
  -- Trying to demonstrate both auth schemes, so use AuthProtect on single user endpoint.
  :<|> AuthProtect "admin" :> Capture "userid" Int :> Get '[JSON] User
  :<|> "filter_age" :> QueryParam "age_less_than" Int :> Get '[JSON] [User]
  :<|> "filter_name" :> QueryParams "name" String :> Get '[JSON] [User]
  :<|> "filter_flag" :> QueryFlag "is_old" :> Get '[JSON] [User]
  :<|> Capture "userid" Int :> ReqBody '[JSON] User :> Put '[JSON] User)

allUsersHandler :: User -> Handler [User]
allUsersHandler user = return allUsers

singleUserHandler :: Int -> Int -> Handler User
singleUserHandler _ uid = case M.lookup uid userMap of
  Nothing -> throwE $ err401 { errBody = "Couldn't find user." }
  Just u -> return u

ageFilterHandler :: Maybe Int -> Handler [User]
ageFilterHandler Nothing = return allUsers
ageFilterHandler (Just maxAge) = return $ filter (\u -> userAge u < maxAge) allUsers

nameFilterHandler :: [String] -> Handler [User]
nameFilterHandler names = return $ filter filterByName allUsers
  where
    filterByName u = userName u `elem` names

flagFilterHandler :: Bool -> Handler [User]
flagFilterHandler isOld = if isOld
  then return $ filter (\u -> userAge u > 24) allUsers
  else return allUsers

updateUserHandler :: Int -> User -> Handler User
updateUserHandler uid newUser = case M.lookup uid userMap of
  Nothing -> throwE $ err401 { errBody = "Couldn't find user." }
  Just _ -> do
    let newMap = M.insert uid newUser
    return newUser

usersServer :: Server UsersAPI
usersServer = allUsersHandler 
  :<|> singleUserHandler 
  :<|> ageFilterHandler 
  :<|> nameFilterHandler
  :<|> flagFilterHandler
  :<|> updateUserHandler

usersAPI :: Proxy UsersAPI
usersAPI = Proxy :: Proxy UsersAPI

type FullAPI = UsersAPI :<|> Raw

fullAPI :: Proxy FullAPI
fullAPI = Proxy :: Proxy FullAPI

docsBS :: ByteString
docsBS = encodeUtf8 . Data.Text.Lazy.pack . markdown $ docs usersAPI

fullServer :: Server FullAPI
fullServer = usersServer :<|> serveDocs
  where
    plain = ("Content-Type", "text/plain")
    serveDocs _ response = response $ responseLBS ok200 [plain] docsBS

instance ToCapture (Capture "userid" Int) where
  toCapture _ = DocCapture "userid" "The ID for the particular user"

instance ToParam (QueryParam "age_less_than" Int) where
  toParam _ = DocQueryParam 
    "age_less_then" 
    ["18", "24", "30"]
    "The upper bound of the age for returned users"
    Normal

instance ToParam (QueryParams "name" String) where
  toParam _ = DocQueryParam
    "name"
    ["John Doe", "Jane Doe"]
    "The names of users you are querying for."
    List

instance ToParam (QueryFlag "is_old") where
  toParam _ = DocQueryParam
    "is_old"
    []
    "A flag filtering if the user is older than 24 years of age."
    Flag

instance ToSample User where
  toSamples _ = singleSample user1

instance ToAuthInfo (BasicAuth "admin" User) where
  toAuthInfo _ = DocAuthentication "A basic authentication scheme" "Requires a username and password"

instance HasDocs (AuthProtect "admin" :> Capture "userid" Int :> Get '[JSON] User) where
  docsFor _ _ _ = emptyAPI

authCheck :: BasicAuthCheck User
authCheck = BasicAuthCheck check
  where
    check (BasicAuthData username password) =
      if username == "James" && password == "admin"
        then return (Authorized $ User "James" "james@mondaymorninghaskell.com" 24)
        else return Unauthorized

authHandler :: AuthHandler Request Int
authHandler = mkAuthHandler handler
  where
    handler request = return 2

authContext :: Context (AuthHandler Request Int ': BasicAuthCheck User ': '[])
authContext = authHandler :. authCheck :. EmptyContext

main :: IO ()
main = run 8000 (serveWithContext fullAPI authContext fullServer)
