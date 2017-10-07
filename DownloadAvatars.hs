{-# LANGUAGE OverloadedStrings #-}

module DownloadAvatars
  where

import Auth
{--
auth :: String
auth = "<username>:<apikey>"
--}

import System.Directory (createDirectoryIfMissing)
import Data.List (intercalate, isSuffixOf)
import Data.Aeson.Lens
import Data.Text (unpack)
import qualified Data.ByteString.Lazy.Char8 as B

import Network.Wreq (get, responseBody)
import Control.Lens

dir :: FilePath
dir = "./avatars"

getAvatars :: String -> String -> IO ()
getAvatars name owner = do
  let url = "https://" ++ auth ++ "@api.github.com/repos/"
        ++ owner ++ "/" ++ name ++ "/contributors"
  r <- get url
  getAvatars' r 0
  where
    getAvatars' r i = do
      let login = r ^? responseBody . nth i . key "login" . _String
      let avatar = r ^? responseBody . nth i . key "avatar_url" . _String
      case (login, avatar) of
        (Just name, Just avatarURL) -> do
          saveAvatar (unpack name) (unpack avatarURL)
          getAvatars' r (i + 1)
        _ -> return ()

saveAvatar :: String -> String -> IO ()
saveAvatar name url = do
  r <- get url
  let body = r ^. responseBody
  let fileName = dir ++ "/" ++ name ++ ".png"
  B.writeFile fileName body
  print name
  print url

main :: IO ()
main = do
  createDirectoryIfMissing False dir
  getAvatars "jquery" "jquery"
