{- messages/data model for Domingo client -}
module DomingoModel exposing(..)

import Dict
import Task
import List
import String
import Json.Encode as Encode
import Json.Decode as Decode exposing(..)
-- this no longer seems to exist
-- import Json.Decode.Extra exposing((|:))

type alias PlayerId = Int
type alias CardId = Int
type alias GameId = String

{- state of each player in a game -}
-- TODO include a player id field?
type alias PlayerState =
    { deck : List CardId
    , discard : List CardId
    , hand : List CardId
    , victory : Int {- Extra VP earned from e.g. actions -}
    , valid : Bool
    }
    -- our implementation of Json.extras.apply
    -- for encoding objects of arbitrary arity
-- usually the Decoder a will be ("var" := val)
(|:) : Decoder (a -> b) -> Decoder a -> Decoder b
(|:) df da =
    -- want to "take out" a -> b, and map it into
    df `andThen` \g -> map g da

playerStateDecoder : Decoder PlayerState
playerStateDecoder =
    succeed PlayerState
        |: ("deck" := Decode.list Decode.int)
        |: ("discard" := Decode.list Decode.int)
        |: ("hand" := Decode.list Decode.int)
        |: ("victory" := Decode.int)
        |: ("valid" := Decode.bool)

-- player state is all just primitives, so we can
-- use default encoding
playerStateEncoder : PlayerState -> Value
playerStateEncoder st =
    Encode.object [ ("deck", Encode.list (List.map Encode.int st.deck))
                  , ("discard", Encode.list (List.map Encode.int st.discard))
                  , ("hand", Encode.list (List.map Encode.int st.hand))
                  , ("victory", Encode.int st.victory)
                  , ("valid", Encode.bool st.valid)
                  ]
           
{- TODO: use continuations to have "response" phases?? -}
-- TODO do we need an encoding?
type GamePhase = PreGamePhase
               | DealPhase
               | ActionPhase
               | CoinPhase
               | BuyPhase
               | EndGamePhase {- game is over -}

-- not sure if this is strictly necessary, but i want to
-- not depend on details of how the language does default printing
gamePhaseDecoder : Decoder GamePhase
gamePhaseDecoder =
    Decode.int |> Decode.map (\i ->
                                    case i of
                                        0 -> PreGamePhase
                                        1 -> DealPhase
                                        2 -> ActionPhase
                                        3 -> CoinPhase
                                        4 -> BuyPhase
                                        5 -> EndGamePhase
                                        _ -> PreGamePhase --shouldn't happen
                               )
                            
gamePhaseEncoder : GamePhase -> Value
gamePhaseEncoder gp =
    Encode.int <|
        case gp of
            PreGamePhase -> 0
            DealPhase -> 1
            ActionPhase -> 2
            CoinPhase -> 3
            BuyPhase -> 4
            EndGamePhase -> 5

{- TODO eventually we should support serialization of cards -}                            
type alias Card =
    { idn : CardId
    , name : String
    , img : Maybe String
    , text : Maybe String
    {- TODO maybe make this semantic. is it action, victory, etc -}
    , kind : String 
    , victory : Int {- how much victory is this worth at end of game? -}
    , spentValue : Int {- how much is it worth if played as money? -}
    , cost : Int {- how much does it cost to buy from the shop? -}
    {- what happens when you play it as an action -}
    , playedEffect : Maybe (GameState -> GameState)
    {- to do: some way of specifying reactions when it's in your hand -}
    {- to do: some way of specifying reactions when it's on the field -}
    }

{- State of a Domingo game in progress.
   This (eventually, deltas to it) are what get serialized over
   the wire and sent to clients
-}
type alias GameState =
    { players : Dict.Dict PlayerId PlayerState
    , playerOrder : List PlayerId
    , shop : Dict.Dict CardId Int
    , trash : List CardId
    {- how many actions, coin, buys the current player has -}
    , actions : Int
    , coin : Int
    , buys : Int
    , purchases : List CardId {- cards purchased that will enter player's discard at end of turn -}
    , plays : List CardId {- cards played this turn that will enter player's discard at end of turn -}
    , phase : GamePhase {- NB this used to be conflated with client state -}
    , rng : Int
    {- used at end of game to display how well people did
    in number of victory points -}
-- winners isn't part of the state but rather calculated from it    
--    , winners : List (PlayerId, Int)
    , gameId : GameId
    }

{- For chaining client actions -}
type alias ClientTask = Task.Task String ClientState

{- Definitions for client-side state -}
-- TODO should these also have constructors?
type alias ClientPreGameState =
  { gidInput : Maybe GameId
  , rngInput : Maybe Int
  , message : Maybe String }

type alias ClientPlayState =
  { gameId : GameId
  , gameState : GameState {- game state should have .gameId == gId -}
  , message : Maybe String
  }

type alias ClientSpectateState =
  { gameId : GameId
  , gameState : Maybe GameState {- Nothing if the server hasn't sent us a message -}
  , message : Maybe String }

type ClientState =
       ClientPreGame ClientPreGameState
     | ClientPlay ClientPlayState
     | ClientSpectate ClientSpectateState

{- Messages used by main app -}
type Msg =  SendToServer String
          | GotServerMsg String
          {- for debugging -}
          | ShowState
          {- useful ones -}
          {- need ones for manually/automatically setting RNG -}
          | UpdateGameId String
          | UpdateSeedToUse String
          -- in pregame state, update seed to use in game
          | StartGame
          | SpectateGame
--        | QuitGame
          | RestartClient
          | PlayCard Int
          | EndPhase
          | BuyCard CardId
          | EndBuy
          {- message sent when new random seed is generated -}
          -- gets randomness if the user did not provide it, and deals initial hand
          | InitRandomAndDeal Int
          {- if a task fails - should be a no op, possibly log error -}
          | TaskFail String
          {- general call to update client's state -}
          | UpdateClientState ClientState
          -- send a message to the server
          | NoOp -- should never be responded to

{- Type for messages sent from client to server -}
type ClientToServerMsg =
       StartGameCMsg GameId
     | SpectateCMsg GameId
     | UpdateGameCMsg GameId GameState

{- Serialization code -}
{- we need to be smarter about this; especially enums -}
clientToServerMsgEncoder : ClientToServerMsg -> Value
clientToServerMsgEncoder msg =
    case msg of
        StartGameCMsg id -> Encode.object [ ("msgType", Encode.string "start")
                                          , ("gId", Encode.string id)]
        SpectateCMsg id -> Encode.object [ ("msgType", Encode.string "spectate")
                                         , ("gId", Encode.string id)]
        UpdateGameCMsg gid gst ->
                Encode.object [ ("msgType", Encode.string "update")
                              , ("gId", Encode.string gid)
                              -- TODO need a function to convert game states to values
                              , ("gameState", gameStateEncoder gst) ]
                           
{- Responses from server -}
-- TODO give this a more reasonable name.
type ServerToClientMsg =
    -- AcknowledgeSMsg
     UpdateGameSMsg GameId GameState

{- JSON decoder for game states -}
-- it would be better to put all these in a sub-module called decoders
-- better yet, have a separate file for communications stuff
-- special thanks to https://robots.thoughtbot.com/decoding-json-structures-with-elm

-- the following are decoders needed to decode the game state back from JSON
-- transform the keys of a dict, specialized to String and Int
-- to get around typechecker limitations (that i believe exist anyway)
-- NB this throws away bindings that fail to parse
-- also, seems inefficient perhaps
dictKeysMap : Dict.Dict String a -> Dict.Dict Int a
dictKeysMap d =
    List.foldl
        (\ks dAcc ->
             -- if we fail to parse, ignore
             case String.toInt ks of
                 Ok ki ->
                     case Dict.get ks d of
                         Just i -> Dict.insert ki i dAcc
                         _ -> dAcc

                 _ -> dAcc
       )
        Dict.empty
        (Dict.keys d)

-- TODO generalize
-- is this even necessary?
dictValuesMap : Dict.Dict Int String -> Dict.Dict Int Int
dictValuesMap d =
    let d' = Dict.map (\k v -> String.toInt v) d in
    Dict.foldl (\k v dAcc ->
                    Dict.update k (\_ -> case v of
                                             Ok i -> Just i
                                             _ -> Nothing)
                        dAcc
               )
        Dict.empty
        d'
           
shopDecoder : Decoder (Dict.Dict CardId Int)
shopDecoder =
    Decode.dict int |> Decode.map dictKeysMap -- |> Decode.map dictValuesMap

shopEncoder : Dict.Dict CardId Int -> Value
shopEncoder d =
    Encode.object <| List.map (\(k,v) -> (toString k, Encode.int v)) <| Dict.toList d 

gameStateDecoder : Decoder GameState
gameStateDecoder =
    (succeed GameState)
        |: ("players" := dict playerStateDecoder |> Decode.map dictKeysMap)
        |: ("playerOrder" := list int)
        |: ("shop" := shopDecoder)
        |: ("trash" := list int)
        |: ("actions" := int)
        |: ("coin" := int)
        |: ("buys" := int)
        |: ("purchases" := list int)
        |: ("plays" := list int)
        |: ("phase" := gamePhaseDecoder)
        |: ("rng" := int)
--        |: ("winners" := list (dict winnersDecoder))
        |: ("gameId" := string)

playersEncoder : Dict.Dict PlayerId PlayerState -> Value
playersEncoder d =
    Encode.object <| List.map (\(k,v) -> (toString k, playerStateEncoder v)) <| Dict.toList d

gameStateEncoder : GameState -> Value
gameStateEncoder gs =
    Encode.object [ ("players", playersEncoder gs.players)
                  , ("playerOrder", Encode.list (List.map Encode.int gs.playerOrder))
                  , ("shop", shopEncoder gs.shop)
                  , ("trash", Encode.list (List.map Encode.int gs.trash))
                  , ("actions", Encode.int gs.actions)
                  , ("coin", Encode.int gs.coin)
                  , ("buys", Encode.int gs.buys)
                  , ("purchases", Encode.list (List.map Encode.int gs.purchases))
                  , ("plays", Encode.list (List.map Encode.int gs.plays))
                  , ("rng", Encode.int gs.rng)
--                  , ("winners", Encode.list (List.map Encode.int gs.winners))
                  , ("gameId", Encode.string gs.gameId)
                  ]

-- TODO can we just use the value decoder and skip all this?
-- gamePhaseDecoder           

{- JSON decoder for server messages -}
serverMsgDecoder : Decoder ServerToClientMsg
serverMsgDecoder =
    -- check based on type, to see what you need
    -- object1 is not what we want here
    object2 UpdateGameSMsg
        -- TODO check to ensure it is the right type
        ("gameId" := string)
        ("gameState" := gameStateDecoder)
                   
{- deserialization code -}
parseServerToClientMessage msg =
    decodeString serverMsgDecoder msg
