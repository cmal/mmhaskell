{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds    #-}

module Client where

import Data.Text
import UsersServer (UsersAPI, usersAPI)
import Servant
import Servant.Client
import Servant.Common.Req
import Types

import Network.HTTP.Client
import Network.HTTP.Client.TLS

type JWToken = Text

type instance AuthClientData (AuthProtect "admin") = JWToken

allUsersClient :: BasicAuthData -> ClientM [User]
singleUserClient :: AuthenticateReq (AuthProtect "admin") -> Int -> ClientM User
ageFilterClient :: Maybe Int -> ClientM [User]
nameFilterClient :: [String] -> ClientM [User]
flagFilterClient :: Bool -> ClientM [User]
updateUserClient :: Int -> User -> ClientM User
(allUsersClient :<|> 
  singleUserClient :<|> 
  ageFilterClient :<|> 
  nameFilterClient :<|>
  flagFilterClient :<|>
  updateUserClient) = client usersAPI

env :: IO ClientEnv
env = do
  manager <- newManager tlsManagerSettings
  url <- parseBaseUrl "http://127.0.0.1:8000"
  return $ ClientEnv manager url

fetchAllUsers :: IO ()
fetchAllUsers = do
  environment <- env
  let authInfo = BasicAuthData "Bad Name" "Bad Password"
  result1 <- runClientM (allUsersClient authInfo) environment 
  result2 <- runClientM (singleUserClient (addJWTHeader "my authtoken") 1) environment 
  print result1
  print result2

addJWTHeader :: JWToken -> AuthenticateReq (AuthProtect "admin")
addJWTHeader jwt = mkAuthenticateReq jwt insertToken

insertToken :: JWToken -> Req -> Req
insertToken jwt req = req { headers = newPair : oldHeaders }
  where
    oldHeaders = headers req
    newPair = ("auth-token", jwt)
