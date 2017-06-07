module Shared where

data Request1 = Request1 Char
data Request2 = Request2 (Char, Char)

data Request
  = Req1 Request1
  | Req2 Request2

