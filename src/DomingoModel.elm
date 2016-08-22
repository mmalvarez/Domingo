{- messages/data model for Domingo client -}
module DomingoModel exposing(..)

import Dict
import Set
import Task
import List
import String
import Json.Encode as Encode
import Json.Decode as Decode exposing(..)
import DomingoLib exposing (..)

type alias PlayerId = String
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

{- dummy player for use with the next function -}
dummyId : PlayerId
-- dear lord this is bad please fix
dummyId = "NOTREAL"

dummy : PlayerState
dummy =
  { deck = []
  , discard = []
  , hand = []
  , victory = 0
  , valid = False
  }
    

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
-- TODO be robust to nonsense packets, rather than default-valuing
-- TODO just use Decoder.fail, I think
-- we need an AndThen
gamePhaseDecoder : Decoder GamePhase
gamePhaseDecoder =
    Decode.int `andThen` \i -> case i of
                                        0 -> succeed PreGamePhase
                                        1 -> succeed DealPhase
                                        2 -> succeed ActionPhase
                                        3 -> succeed CoinPhase
                                        4 -> succeed BuyPhase
                                        5 -> succeed EndGamePhase
                                        _ -> fail "invalid game phase"
                            
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

{- Describes additional choices the current player sometimes must take -}
{- Eventually this will have "pick card from list" etc -}
type GamePrompt = NoPrompt -- "normal" state, no prompt
                | AcceptDecline -- for "may" effects

gamePromptDecoder : Decoder GamePrompt
gamePromptDecoder =
    Decode.int `andThen` \i -> case i of
                                        0 -> succeed NoPrompt
                                        1 -> succeed AcceptDecline
                                        _ -> fail "invalid prompt type"
                  
gamePromptEncoder : GamePrompt -> Value
gamePromptEncoder gp =
    Encode.int <|
        case gp of
            NoPrompt -> 0
            AcceptDecline -> 1

{- State of a Domingo game in progress.
   This (eventually, deltas to it) are what get serialized over
   the wire and sent to clients
-}
type alias GameState =
    { gameId : GameId
    , players : Dict.Dict PlayerId PlayerState
    , playerOrder : List PlayerId
    , shop : Dict.Dict CardId Int
    , trash : List CardId
    {- how many actions, coin, buys the current player has -}
    , actions : Int
    , coin : Int
    , buys : Int
    , purchases : List CardId {- cards purchased that will enter player's discard at end of turn -}
    , plays : List CardId {- cards played this turn that will enter player's discard at end of turn -}
    , phase : GamePhase
    , rng : Int
    , prompt : GamePrompt -- if the current player must take some kind of special action
    }

{- For chaining client actions -}
type alias ClientTask = Task.Task String ClientState

{- Definitions for client-side state -}
-- TODO should these also have constructors?
type alias ClientPreGameState =
  { gidInput : Maybe GameId
  , newPidInput : Maybe PlayerId
  , pidsInput : List PlayerId -- List to support multiplayer on same client. If empty we are a spectator
  , message : Maybe String }

    -- describe what type of lobby this is (play or spectate)
type LobbyType = LobbyPlay
               | LobbySpectate

-- config options specific to the master                 
type alias ClientLobbyConfigState =
    { rngInput : Maybe Int
    -- TODO other config options, like cards
    -- we also need to include a way to kick/restrict access
    }
        
-- TODO: include information such as
-- how many people are connected, is host connected, etc
type alias ClientLobbyState =
    { gameId : GameId
    -- players on this client
    , localPlayerIds : List PlayerId
    -- all players in game (for display purposes
    , playerIds : List PlayerId
    -- am I master? if so, what is config state
    , masterConfigState : Maybe ClientLobbyConfigState
    -- has the server responded
    , serverResponded : Bool
    -- what players (incl master) there are.
    , playerIds : List PlayerId
    , message : Maybe String
    }

type alias ClientPlayState =
  { gameId : GameId
  , localPlayerIds : Set.Set PlayerId -- keep track of whose turns should be taken on this machine
  , gameState : GameState {- game state should have .gameId == gId -}
  , message : Maybe String
  }

type ClientState =
       ClientPreGame ClientPreGameState
     | ClientLobby ClientLobbyState
     | ClientPlayMaster ClientPlayState
     | ClientPlaySub ClientPlayState

{- Messages used by main app -}
type Msg =
          {- communications -}
            GotServerMsg String
          {- for debugging -}
          | ShowState
          {- pregame. -}
          | UpdateGameId GameId
          | UpdateNewPlayer PlayerId            
          | AddNewPlayer
          | RemovePlayer PlayerId
          | StartLobby -- True if we are spectator
          {- lobby -}
          | UpdateMasterSeed String
          | StartGame
          {- in game -}
          | SubmitMove PlayDesc -- master and nonmaster use this
          | DoMove MoveDesc -- only master uses this
          {- miscellaenous -}
          {- message sent when new random seed is generated 
             gets randomness if the user did not provide it, and deals initial hand -}
          | FinishStartingGame Int
          {- if a task fails - should be a no op, possibly log error. not sure if i even use this... -}
          | TaskFail String
          {- general call to update client's state, and send it to the server (todo: rename; this is a master-only thing) -}
--          | UpdateClientState ClientState
          | BroadcastGameState
          | NoOp -- should never be responded to
          | RestartClient

{- Description of possible moves; used to serialize/deserialize moves between
   server and clients -}
type PlayDesc = PlayCard Int
              | BuyCard Int
              | EndPhase
              | Accept
              | Decline
type alias MoveDesc = {playerId : PlayerId, play : PlayDesc}

moveDescEncoder : MoveDesc -> Value
moveDescEncoder md = case md.play of
                         PlayCard i -> Encode.object [("moveType", Encode.string "play")
                                                     ,("card", Encode.int i)
                                                     ,("player", Encode.string md.playerId)]
                         BuyCard i -> Encode.object [("moveType", Encode.string "buy")
                                                    ,("card", Encode.int i)
                                                    ,("player", Encode.string md.playerId)]
                         EndPhase -> Encode.object [("moveType", Encode.string "endPhase")
                                                    ,("player", Encode.string md.playerId)]
                         Accept -> Encode.object [("moveType", Encode.string "accept")
                                                        ,("player", Encode.string md.playerId)]
                         Decline -> Encode.object [("moveType", Encode.string "decline")
                                                         ,("player", Encode.string md.playerId)]

-- OK this gets slightly more annoying now that we have to pull it out
-- TODO: under construction
moveDescDecoder : Decoder MoveDesc
moveDescDecoder =
    ("player" := string) `andThen` \playerId ->
        let mkWithCardInput p c = {play = p c, playerId = playerId}
        in
        ("moveType" := string) `andThen` \moveType ->
            case moveType of
                "play" -> object1 (mkWithCardInput PlayCard) ("card" := int)
                "buy" -> object1 (mkWithCardInput BuyCard) ("card" := int)
                "endPhase" -> succeed {play = EndPhase, playerId = playerId}
                "accept" -> succeed {play = Accept, playerId = playerId}
                "decline" -> succeed {play = Decline, playerId = playerId}
                _ -> fail "invalid move"
                 
{- Type for messages sent from client to server -}
type ClientToServerMsg =
       JoinLobbyCMsg GameId (List PlayerId)
     | StartGameCMsg GameId -- TODO I think this one might be superfluous
     | QuitGameCMsg {- GameId -}
     | MakeMoveCMsg GameId MoveDesc 
     | MasterPushCMsg GameId GameState
     -- TODO is GameId necessary
     -- takes GameId of game, and list of _all_ players in game
     | MasterLobbyPushCMsg GameId (List PlayerId)

{- Serialization code -}
clientToServerMsgEncoder : ClientToServerMsg -> Value
clientToServerMsgEncoder msg =
    case msg of
        JoinLobbyCMsg gid ps ->
            Encode.object [ ("msgType", Encode.string "join")
                          , ("gameId", Encode.string gid)
                          , ("players", Encode.list (List.map Encode.string ps)) ]
                              
        StartGameCMsg gid -> Encode.object [ ("msgType", Encode.string "start")
                                           , ("gameId", Encode.string gid) ]

        MakeMoveCMsg gid md -> Encode.object [ ("msgType", Encode.string "makeMove")
                                             , ("move", moveDescEncoder md)
                                             , ("gameId", Encode.string gid)]
                                
        MasterPushCMsg gid gst ->
            Encode.object [ ("msgType", Encode.string "masterPush")
                          , ("gameId", Encode.string gid)
                          , ("gameState", gameStateEncoder gst) ]

        MasterLobbyPushCMsg gid ps->
            Encode.object [ ("msgType", Encode.string "masterLobbyPush")
                          , ("gameId", Encode.string gid)
                          , ("players", Encode.list (List.map Encode.string ps))]

        QuitGameCMsg ->
            Encode.object [ ("msgType", Encode.string "quit") ]

type ServerLobbyResponse = LobbyMaster -- if you are the first one in (hence, master)
                         | LobbyClient -- if you join an active lobby
                         | LobbyFail -- if you are trying to e.g. join a game that already started,
                                     -- or choose a player name already taken
                           

{- Responses from server -}
type ServerToClientMsg = PlayerJoinedSMsg GameId (List PlayerId)
                       | MadeMoveSMsg GameId MoveDesc -- used to inform master of another user's move
                       | UpdateGameStateSMsg GameId GameState String -- receive master's pushed game state
                       -- lets the client know they were the first one in and
                       -- thus are the "master"
                       -- server responses to attempts to join lobby
                       | LobbyResponseSMsg ServerLobbyResponse
                       -- update lobby state with new player list
                       | UpdateLobbySMsg (List PlayerId)
                       | PlayerQuitSMsg -- TODO include more info?? maybe a player ID or something
                       | UnknownSMsg -- used in event of parse failure; eventually remove

{- JSON decoder for game states -}
{- special thanks to https://robots.thoughtbot.com/decoding-json-structures-with-elm -}

-- transform the keys of a dict, specialized to String and Int
-- NB this throws away bindings that fail to parse
-- perhaps inefficent; probably unused now
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
        |: ("gameId" := string)
        |: ("players" := dict playerStateDecoder {- |> Decode.map dictKeysMap -})
        |: ("playerOrder" := list string)
        |: ("shop" := shopDecoder)
        |: ("trash" := list int)
        |: ("actions" := int)
        |: ("coin" := int)
        |: ("buys" := int)
        |: ("purchases" := list int)
        |: ("plays" := list int)
        |: ("phase" := gamePhaseDecoder)
        |: ("rng" := int)
        |: ("prompt" := gamePromptDecoder)

playersEncoder : Dict.Dict PlayerId PlayerState -> Value
playersEncoder d =
    Encode.object <| List.map (\(k,v) -> (k, playerStateEncoder v)) <| Dict.toList d

gameStateEncoder : GameState -> Value
gameStateEncoder gs =
    Encode.object [ ("gameId", Encode.string gs.gameId)
                  , ("players", playersEncoder gs.players)
                  , ("playerOrder", Encode.list (List.map Encode.string gs.playerOrder))
                  , ("shop", shopEncoder gs.shop)
                  , ("trash", Encode.list (List.map Encode.int gs.trash))
                  , ("actions", Encode.int gs.actions)
                  , ("coin", Encode.int gs.coin)
                  , ("buys", Encode.int gs.buys)
                  , ("purchases", Encode.list (List.map Encode.int gs.purchases))
                  , ("plays", Encode.list (List.map Encode.int gs.plays))
                  , ("phase", gamePhaseEncoder gs.phase)
                  , ("rng", Encode.int gs.rng)
                  , ("prompt", gamePromptEncoder gs.prompt)
                  ]

-- decode server lobby responses
-- again, maybe also use 'fail' here
lobbyResponseDecoder : Decoder ServerLobbyResponse
lobbyResponseDecoder =
    ("response" := string) `andThen` \s ->
        case s of
            "lobbyMaster" -> succeed LobbyMaster
            "lobbyClient" -> succeed LobbyClient
            "lobbyFail" -> succeed LobbyFail
            _ -> succeed LobbyFail


{- JSON decoder for server messages -}
-- TODO: use 'fail' instead of having Unknown S Msg?
serverToClientMsgDecoder : Decoder ServerToClientMsg
serverToClientMsgDecoder =
    -- check based on type, to see what you need
    ("msgType" := string) `andThen`
        \s -> case s of
                      
                  "madeMove" ->
                      object2 MadeMoveSMsg
                          ("gameId" := string)
                          ("move" := moveDescDecoder)
                  
                  "updateGameState" ->
                      object3 UpdateGameStateSMsg
                          ("gameId" := string)
                          ("gameState" := gameStateDecoder)
                          ("message" := string)

                  "playerJoined" ->
                      object2 PlayerJoinedSMsg
                          ("gameId" := string)
                          ("players" := list string)

                  "playerQuit" ->
                      succeed PlayerQuitSMsg

                  "lobbyResponse" ->
                      object1 LobbyResponseSMsg lobbyResponseDecoder

                  "updateLobbyState" ->
                      object1 UpdateLobbySMsg
                          ("players" := list string)

                  _ -> succeed UnknownSMsg
    
                   
{- deserialization code -}
parseServerToClientMessage msg =
    decodeString serverToClientMsgDecoder msg
