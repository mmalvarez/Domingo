{- DomingoConf.elm, contains parameters a user might want to tune -}
module DomingoConf exposing(..)

import DomingoModel exposing(..)
import DomingoCards exposing(..)
import DomingoLib exposing(..)
import DomingoCardPrimitives exposing(..)
import Dict

startingDeck : List CardId
startingDeck = List.repeat 7 copperId ++ List.repeat 3 estateId

{- state of a player at the beginning -}
{- TODO make different ones for different players, with different IDs -}
startingPlayerState : PlayerState
startingPlayerState =
  { deck = []
  , discard = startingDeck, hand = [], victory = 0
  , valid = True
  }

handSize = 5

{- initial numbers for cards -}
estateHowMany : Int
estateHowMany = 1

copperHowMany : Int
copperHowMany = 50

woodcutterHowMany : Int
woodcutterHowMany = 10

villageHowMany : Int
villageHowMany = 10

-- if the rng is ever set to this value it will be assumed uninitialized
rngBogusValue : Int
rngBogusValue = 0

startingGameState : GameState
startingGameState =
  { players = Dict.fromList [(dummyId, startingPlayerState)]
  , playerOrder = [dummyId]
  , shop = Dict.fromList [(estateId, estateHowMany), (copperId, copperHowMany),
                          (woodcutterId, woodcutterHowMany), (villageId, villageHowMany)]
  , trash = []
  , actions = 0, coin = 0, buys = 0
  , purchases = [], plays = []
  , phase = PreGamePhase
  , rng = rngBogusValue
--  , winners = []
--  , message = "" -- TODO eventually get rid of this?
  , gameId = ""
  }

{- predicate checking for game over (called after each purchase) -}
gameOver : GameState -> Bool

{- game ends if there are no more estates -}
gameOver state =
  let numEstates = dflGet estateId state.shop 0 in
  if numEstates == 0 then True
    else False
      
