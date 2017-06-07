module Main where

import Shared

main :: IO ()
main = undefined

getResponse' :: Shared.Request -> Char
getResponse' a =
  case a of
    (Req1 r) ->  'c' --getResponse a r
--getResponse' a@(Req2 r) = toJSON $ getResponse a r

