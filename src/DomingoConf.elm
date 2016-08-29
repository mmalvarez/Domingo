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
{- TODO all of these but kingdom cards depend on number of players -}
estateHowMany : Int
estateHowMany = 1

copperHowMany : Int
copperHowMany = 50

silverHowMany : Int
silverHowMany = 40

goldHowMany : Int
goldHowMany = 30

actionHowMany : Int                
actionHowMany = 10

-- if the rng is ever set to this value it will be assumed uninitialized
rngBogusValue : Int
rngBogusValue = 0

-- TODO make this configurable!!
startingGameState' =
    { players = Dict.fromList [(dummyId, startingPlayerState)]
    , playerOrder = [dummyId]
    , shop = Dict.fromList [(copperId, copperHowMany)
                           ,(silverId, silverHowMany)
                           ,(goldId, goldHowMany)
                           ,(estateId, estateHowMany)
                           ,(woodcutterId, actionHowMany)
                           ,(villageId, actionHowMany)
                           ]
    , trash = []
    , actions = 0, coin = 0, buys = 0
    , purchases = [], plays = []
    , phase = PreGamePhase
    , rng = rngBogusValue
    , gameId = ""
    , prompt = Nothing
    , cont = Nothing
    }

-- TODO move this out, let users apply changes to it
startingGameState : GameState
startingGameState =
  GameState startingGameState'

{- predicate checking for game over (called after each purchase) -}
{- game ends if there are no more Provinces
   (TODO: add 3-pile ending)
 -}
gameOver : GameState -> Bool
gameOver gst =
    case gst of
        GameState st ->
            let numEstates = dflGet provinceId st.shop 0 in
            if numEstates == 0 then True
            else False
      
