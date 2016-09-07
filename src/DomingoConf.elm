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
{- int is number of players -}
victoryHowMany : Int -> Int
victoryHowMany numPlayers =
    if numPlayers <= 2 then 8
    else 12

curseHowMany : Int -> Int
curseHowMany numPlayers =
    if numPlayers <= 2 then 10
    else if numPlayers == 3 then 20
    else 30

copperHowMany : Int
copperHowMany = 60

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
-- maybe just provide the starting players list as input?
startingGameState' {-numPlayers-} playerIds =
    let numPlayers = List.length playerIds
        victoryHowMany' = victoryHowMany numPlayers
        curseHowMany' = curseHowMany numPlayers
    in
    { players = initPlayers startingPlayerState playerIds
    , playerOrder = playerIds
    , shop = Dict.fromList
             [ -- money
              (copperId, copperHowMany)
             ,(silverId, silverHowMany)
             ,(goldId, goldHowMany)
             -- victory
             ,(estateId, victoryHowMany')
             ,(duchyId, victoryHowMany')
             ,(provinceId, victoryHowMany')
             ,(gardensId, victoryHowMany')
             ,(curseId, curseHowMany')
             -- actions
             ,(woodcutterId, actionHowMany)
             ,(villageId, actionHowMany)
             ,(smithyId, actionHowMany)
             ,(marketId, actionHowMany)
             ,(festivalId, actionHowMany)
             ,(witchId, actionHowMany)
             ,(throneRoomId, actionHowMany)
             ,(laboratoryId, actionHowMany)
             ,(councilRoomId, actionHowMany)
             ,(workshopId, actionHowMany)
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
startingGameState : (List PlayerId) -> GameState
startingGameState playerIds =
  GameState <| startingGameState' playerIds

{- predicate checking for game over (called after each purchase, should be each gain.) -}
{- TODO: add a "gain" primitive we can add this check to -}
gameOver : GameState -> Bool
gameOver gst =
    case gst of
        GameState st ->
            let noProvinces = Dict.get provinceId st.shop == Just 0
                numEmpty =
                    Dict.foldl (\_ num acc ->
                                    if num == 0 then acc + 1 else acc
                               ) 0 st.shop
            in
            noProvinces || numEmpty >= 3
                
      
