{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Main where

import Reflex.WebSocket.WithWebSocket.Shared
import qualified Data.Text as T
import Data.Monoid
import Shared
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Network.WebSockets
import Data.Aeson
import Data.Maybe (fromMaybe, catMaybes)

main :: IO ()
main = runServer "127.0.0.1" 3000 app

app c = do
  conn <- acceptRequest c
  let loop = do
        d <- receiveData conn
        print d
        -- let resp = handleRequest d
        resp <- handleRequest d
        print resp
        sendBinaryData conn resp
        loop
  loop

handleRequest :: ByteString -> IO ByteString
handleRequest bstr =
  case decodeStrict bstr of
    (Just (v, r)) -> do
      resp <- getResponse r
      return $ toStrict $ encode $ (v :: String, resp)
    _ -> error "Cannot decode request"

getResponse :: Shared.Request -> IO Value
getResponse r = runHandlerTop r handler

handler =
  makeHandler getResp1
  :<&> makeHandler getResp2
  :<&> makeHandler getResp3

  where
  makeHandler
    :: (WebSocketMessage Shared.Request a, Monad m)
    => (a -> m (ResponseT Shared.Request a)) -> Handler m Shared.Request a
  makeHandler = Handler req
    where req = undefined :: Shared.Request

getResp1 :: Request1 -> IO Response1
getResp1 (Request1 t) = return $ Response1 (T.length t)

getResp2 :: Request2 -> IO Response2
getResp2 (Request2 (t1,t2)) = return $ Response2 (t1 <> t2)

getResp3 :: Request3 -> IO Response3
getResp3 (Request3 ts) = return $ Response3 ("Length", length ts)

data a :<&> b = a :<&> b
infixr 3 :<&>

data Handler m req a where
  Handler :: (WebSocketMessage req a, Monad m) =>
    req -> (a -> m (ResponseT req a )) -> Handler m req a

class IsValidHandler m req r h where
  runHandler :: req -> r -> h -> m Value

instance (WebSocketMessage req a
  , IsValidHandler m req b c) => IsValidHandler m req (a :<|> b) ((Handler m req a) :<&> c) where
  runHandler req (Terminal a) ((Handler _ h) :<&> _) = h a >>= (\x -> return $ toJSON x)
  runHandler req (Recurse b) (_ :<&> c) = runHandler req b c

instance (WebSocketMessage req a) => IsValidHandler m req a (Handler m req a) where
  runHandler req a (Handler _ h) = h a >>= (\x -> return $ toJSON x)

runHandlerTop :: (IsValidHandler m req req h) => req -> h -> m Value
runHandlerTop req h = runHandler req req h
