port module DomingoPorts exposing (..)

import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing(..)
import String

import DomingoModel exposing(..)

{- ports, for use with communicating with socket.io -}
{-
main =
  App.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }
  -}

{- TODO use a real message type!! -}
port toServer : String -> Cmd msg

port fromServer : (String -> msg) -> Sub msg