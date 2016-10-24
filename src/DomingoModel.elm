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
type alias PromptId = Int

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
type Card = Card
    { idn : CardId
    , name : String
    , img : Maybe String
    , text : Maybe String
    {- TODO maybe make this semantic. is it action, victory, etc -}
    , kind : String 
    , victory : PlayerState -> Int {- how much victory is this worth at end of game? -}
    , spentValue : Int {- how much is it worth if played as money? -}
    , cost : Int {- how much does it cost to buy from the shop? -}
    {- what happens when you play it as an action
       we take a list of cards to break a circularity
       gah, this means Card can't be a type alias
     -}
    , playedEffect : Maybe (Dict.Dict CardId Card -> GameState -> GameState)
    {- after we play the card, where does it go/what else happens -}
    , afterPlayEffect : GameState -> GameState
    {- to do: some way of specifying reactions when it's in your hand -}
    {- to do: some way of specifying reactions when it's on the field -}
    }

unwrapCard c' =
    case c' of
        Card c -> c

-- we also need encoder/decoder for this
type alias GamePrompt =
    { spec : PromptSpec
    , playerId : PlayerId
    , desc : String -- UI description; TODO have it be an HTML?
    }

gamePromptEncoder : Maybe GamePrompt -> Value
gamePromptEncoder mgp =
    case mgp of
        Nothing -> Encode.object [("type", Encode.string "gamePrompt_nothing")]
        Just gp -> Encode.object
                   [("type", Encode.string "gamePrompt_just")
                   ,("spec", promptSpecEncoder gp.spec)
                   ,("playerId", Encode.string gp.playerId)
                   ,("desc", Encode.string gp.desc)]

gamePromptDecoder : Decoder (Maybe GamePrompt)
gamePromptDecoder =
    ("type" := string) `andThen` \s ->
        case s of
            "gamePrompt_just" ->
                    object3 (\x y z -> Just (GamePrompt x y z))
                        ("spec" := promptSpecDecoder)
                        ("playerId" := string)
                        ("desc" := string)
                            
            _ -> succeed Nothing

-- describes the types of prompts we might ask user
-- NB strings are how we tell the user what the prompt means
type PromptSpec =
      AcceptDecline
    -- Bool in the following two is whether to allow user to decline
    | ChooseCard Bool (List CardId)
    -- bool = declinable. list is a list of shop cards to include
    -- if the list is empty this means choose from entire hand/shop
    | ChooseHandCard Bool (List CardId)
    | ChooseShopCard Bool (List CardId)
    | Unknown

promptSpecEncoder : PromptSpec -> Value
promptSpecEncoder p =
    case p of
        AcceptDecline -> Encode.object [("type", Encode.string "acceptDecline")]
        ChooseCard b cs -> Encode.object
                           [("type", Encode.string "chooseCard")
                           ,("declinable", Encode.bool b)
                           ,("cards", Encode.list (List.map Encode.int cs))]
        ChooseHandCard b cs ->
            Encode.object [("type", Encode.string "chooseHandCard")
                          ,("declinable", Encode.bool b)
                          ,("card", Encode.list (List.map Encode.int cs))]

        ChooseShopCard b cs -> Encode.object
                               [("type", Encode.string "chooseShopCard")
                               ,("declinable", Encode.bool b)
                               ,("cards", Encode.list (List.map Encode.int cs))]

        Unknown -> Encode.object [("type", Encode.string "unknown")]

promptSpecDecoder : Decoder PromptSpec
promptSpecDecoder =
    ("type" := string) `andThen` \s ->
        case s of
            "acceptDecline" -> succeed AcceptDecline
            "chooseCard" -> object2 ChooseCard ("declinable" := bool) ("cards" := list int)
            "chooseHandCard" -> object2 ChooseHandCard ("declinable" := bool) ("cards" := list int)
            "chooseShopCard" -> object2 ChooseShopCard ("declinable" := bool) ("cards" := list int)
            _ -> succeed Unknown

-- this is used to submit prompt responses to the server
-- TODO use failure instead of "unknown"
type PromptOutput =
      POBool Bool
    | POInt Int
    | POMaybeInt (Maybe Int)
    | POUnknown

promptOutputEncoder : PromptOutput -> Value
promptOutputEncoder po =
    case po of
        POBool b -> Encode.object [ ("type", Encode.string "bool")
                                  , ("value", Encode.bool b) ]
        POInt i -> Encode.object [ ("type", Encode.string "int")
                                 , ("value", Encode.int i)]
        POMaybeInt (Just i) -> Encode.object [ ("type", Encode.string "maybeInt_just")
                                             , ("value", Encode.int i) ]
        POMaybeInt Nothing -> Encode.object [ ("type", Encode.string "maybeInt_nothing") ]

        POUnknown -> Encode.object [("type", Encode.string "unknown")]
                              

promptOutputDecoder : Decoder PromptOutput
promptOutputDecoder = ("type" := string) `andThen` \s ->
                      case s of
                          "bool" -> object1 POBool ("value" := bool)
                          "int" -> object1 POInt ("value" := int)
                          "maybeInt_just" -> object1 (\x -> POMaybeInt (Just x)) ("value" := int)
                          "maybeInt_nothing" -> succeed (POMaybeInt Nothing)
                          _ -> succeed POUnknown

{- State of a Domingo game in progress.
   This (eventually, deltas to it) are what get serialized over
   the wire and sent to clients
   WITH the exception of the prompt, which contains a continuation
-}
type GameState = GameState
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
    -- if the current player must take some kind of special action
    -- PlayerId is whose action it is, String is a description
    , prompt : Maybe GamePrompt 
    -- used to modify the games state based on prompt output
    -- NB: why do we take a game state as input? because this allows us to keep the same
    --   prompt easily. the game state is the "starting" state.
    -- this will always be None on the remote
    , cont : Maybe (PromptOutput -> GameState -> GameState)
    }

-- helper for deconstructing game states
unwrapGameState gs' =
    case gs' of
        GameState gs -> gs

-- helper for cleaning up after prompt runs
clearPrompt : GameState -> GameState
clearPrompt gs' =
    case gs' of
        GameState gs ->
            GameState {gs | prompt = Nothing, cont = Nothing}

-- convenience wrapper for creating gamestates, used e.g. in decoder
makeGameState gameId players playerOrder shop trash actions coin buys purchases plays phase rng prompt cont =
    GameState { gameId = gameId
              , players = players
              , playerOrder = playerOrder
              , shop = shop
              , trash = trash
              , actions = actions
              , coin = coin
              , buys = buys
              , purchases = purchases
              , plays = plays
              , phase = phase
              , rng = rng
              , prompt = prompt
              , cont = cont }

-- unwrap the game state
getGameState ogst =
    case ogst of
        GameState gst -> gst

{- Client state, minus the prompt (which cannot be reified) -}
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
    -- all players in game (for display purposes)
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
  , gameLog : List LogEntry -- messages describing all plays so far, plus other stuff (e.g. chat)
  , chatBox : Maybe String
  , message : Maybe String
  }

-- events in the game. e.g. game start, game end
-- TODO add beginning/end of turns to this.
type GameEvent =
      GameStart Int -- this is the RNG value used to start the game. maybe we don't want it.
    | GameEnd
    -- TurnStart (turnNum, playerId). TODO we need to add turn number to game.

gameEventEncoder : GameEvent -> Value
gameEventEncoder ge =
    case ge of
        GameStart rng -> Encode.object [("eventType", Encode.string "gameStart")
                                       ,("rng", Encode.int rng)]
        GameEnd -> Encode.object [("eventType", Encode.string "gameEnd")]

gameEventDecoder : Decoder GameEvent
gameEventDecoder =
    ("eventType" := string) `andThen` \eventType ->
        case eventType of
            "gameStart" -> object1 GameStart ("rng" := int)
            "gameEnd" -> succeed GameEnd
            _ -> fail "invalid game event"

-- type for entries that go in the game log.
type LogEntry = LoggedChatMessage PlayerId String
              | LoggedMove MoveDesc
              | LoggedGameEvent GameEvent

logEntryEncoder : LogEntry -> Value
logEntryEncoder le =
    case le of
        LoggedChatMessage pid m -> Encode.object [("entryType", Encode.string "loggedChat")
                                                    ,("playerId", Encode.string pid)
                                                    ,("message", Encode.string m)]

        LoggedMove md -> Encode.object [("entryType", Encode.string "loggedMove")
                                       ,("move", moveDescEncoder md)]

        LoggedGameEvent ge -> Encode.object [("entryType", Encode.string "loggedGameEvent")
                                            ,("event", gameEventEncoder ge)]

-- TODO we should make a decoder/encoder for e.g. player id
-- in case we want to change the type
logEntryDecoder : Decoder LogEntry
logEntryDecoder =
    ("entryType" := string) `andThen` \entryType ->
        case entryType of
            "loggedChat" ->
                object2
                    LoggedChatMessage
                    ("playerId" := string) ("message" := string)
            "loggedMove" ->
                object1 LoggedMove ("move" := moveDescDecoder)

            "loggedGameEvent" ->
                object1 LoggedGameEvent ("event" := gameEventDecoder)
                    
            _ -> fail "invalid log entry"
    

type ClientState =
       ClientPreGame ClientPreGameState
     | ClientLobby ClientLobbyState
     | ClientPlayMaster ClientPlayState
     | ClientPlaySub ClientPlayState

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
        -- for chat messages
        | SendChat
        -- for composing chat messages
        | UpdateChatBox String
        | NoOp -- should never be responded to
        | RestartClient
          
{- Description of possible moves; used to serialize/deserialize moves between
   server and clients -}
type PlayDesc = PlayCard CardId
              | BuyCard CardId
              | EndPhase
              | PromptResponse PromptOutput


-- TODO: have a Related "event" type that is this plus a turn
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
                         PromptResponse po ->
                             Encode.object [("moveType", Encode.string "promptResponse")
                                           ,("output", promptOutputEncoder po)
                                           ,("player", Encode.string md.playerId)]
                                           
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
                "promptResponse" ->
                    object1 (\po -> {play = PromptResponse po, playerId = playerId})
                        ("output" := promptOutputDecoder)
                    
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
     -- log a move or send a chat
     -- TODO something to prevent spoofing moves
     | GameLogCMsg GameId LogEntry


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

        MasterLobbyPushCMsg gid ps ->
            Encode.object [ ("msgType", Encode.string "masterLobbyPush")
                          , ("gameId", Encode.string gid)
                          , ("players", Encode.list (List.map Encode.string ps))]

        GameLogCMsg gid le ->
            Encode.object [ ("msgType", Encode.string "gameLog")
                          , ("gameId", Encode.string gid)
                          , ("logEntry", logEntryEncoder le) ]

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
                       -- message for getting sent logged moves/chats
                       | GameLogSMsg GameId LogEntry
                       | UnknownSMsg -- used in event of parse failure; eventually remove

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

{- JSON decoder for game states -}
{- special thanks to https://robots.thoughtbot.com/decoding-json-structures-with-elm -}

gameStateDecoder : Decoder GameState
gameStateDecoder =
    (succeed makeGameState)
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
        |: (succeed Nothing) -- we can't serialize code, nor do we need to

playersEncoder : Dict.Dict PlayerId PlayerState -> Value
playersEncoder d =
    Encode.object <| List.map (\(k,v) -> (k, playerStateEncoder v)) <| Dict.toList d

gameStateEncoder : GameState -> Value
gameStateEncoder gso =
    case gso of
        GameState gs ->
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

                  "gameLog" ->
                      object2 GameLogSMsg
                          ("gameId" := string)
                          ("logEntry" := logEntryDecoder)

                  _ -> succeed UnknownSMsg
    
                   
{- deserialization code -}
parseServerToClientMessage msg =
    decodeString serverToClientMsgDecoder msg
