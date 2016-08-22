import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(..)
import Html.App
import Dict
import Set
import List
import Random
import Task exposing (succeed)
import String exposing (toInt)
import Json.Decode as Decode
import Json.Encode as Encode

{- import the rest of Domingo -}
import DomingoLib exposing(..)
import DomingoModel exposing (..)
import DomingoPorts exposing(..)
import DomingoCardPrimitives exposing(..)
import DomingoCards exposing(..)
import DomingoView exposing (..)
import DomingoConf exposing(..)


{- TODO: Fix div structure of display functions -}
{- TODO: implement initial deal more sanely,
   use updateState -}
{- TODO: More logically divide between Domingo,
   DomingoActions, and DomingoConfig -}


startingClientState : ClientState
startingClientState = ClientPreGame
  { gidInput = Nothing
  , newPidInput = Nothing
  , pidsInput = []
  , message = Nothing
  }

main =
  Html.App.program
    { init = init
    , view = DomingoView.displayClientState
    , update = update
    , subscriptions = subscriptions
  }

init : (ClientState, Cmd Msg)
init = (startingClientState, Cmd.none)

pushMsg : ClientToServerMsg -> Cmd Msg
pushMsg m =
    toServer <| Encode.encode 0 <| clientToServerMsgEncoder m

updateAndPushMasterState : ClientState -> GameState -> (ClientState, Cmd Msg)
updateAndPushMasterState cs gs =
    case cs of
        ClientPlayMaster cp ->
            (ClientPlayMaster {cp | gameState = gs}, pushMsg (MasterPushCMsg gs.gameId gs))
        _ -> (cs, Cmd.none)

-- send an internal message
sendInternalMsg : Msg -> Cmd Msg
sendInternalMsg m = Task.perform (\_ -> NoOp) (\_ -> m) (Task.succeed ())

{- Respond to messages -}
update : Msg -> ClientState -> (ClientState, Cmd Msg)
update msg state =
  case (msg, state) of

    (ShowState, _) ->
      (Debug.log (toString state) state, Cmd.none)
          
    {- pregame -}
    (UpdateGameId gId, ClientPreGame cst) ->
        (ClientPreGame {cst | gidInput = Just gId}, Cmd.none)

    (UpdateNewPlayer pId, ClientPreGame cst) ->
        (ClientPreGame {cst | newPidInput = Just pId}, Cmd.none)

    (AddNewPlayer, ClientPreGame cst) ->
        let pidsInput' =
                -- did they enter anything?
                case cst.newPidInput of
                    Nothing -> cst.pidsInput
                    Just p ->
                        if p /= "" then
                            -- player IDs must be unique
                            if List.member p cst.pidsInput
                            then cst.pidsInput
                            else cst.pidsInput ++ [p]
                        else cst.pidsInput
        in
        (ClientPreGame {cst | pidsInput = pidsInput'}, Cmd.none)

    (RemovePlayer pId, ClientPreGame cst) ->
        let pidsInput' =
                List.filter (\p -> not (p == pId)) cst.pidsInput
        in
        (ClientPreGame {cst | pidsInput = pidsInput'}, Cmd.none)

    -- TODO wait for server response to see if we are master
    (StartLobby, ClientPreGame cpst) ->
        -- they need to provide us with a game id
        case cpst.gidInput of
            Nothing -> (ClientPreGame cpst, Cmd.none)
            Just gid ->
                let pidsInput' =
                        case cpst.newPidInput of
                            Nothing -> cpst.pidsInput
                            Just p ->
                                if p /= "" then
                                    cpst.pidsInput ++ [p]
                                else cpst.pidsInput
                in
                if gid /= "" then
                    -- don't go into Lobby state get, wait for server confirmation
                    (state, pushMsg
                         (JoinLobbyCMsg gid pidsInput'))
                else (ClientPreGame cpst, Cmd.none)

    {- game lobby -}
    (UpdateMasterSeed seedStr, ClientLobby clst) ->
        case (toInt seedStr) of
            Ok seed -> case clst.masterConfigState of
                           Nothing -> (ClientLobby clst, Cmd.none)
                           Just clcst -> (ClientLobby {clst | masterConfigState =
                                                           Just {clcst | rngInput = Just seed}}, Cmd.none)
            Err _ -> (ClientLobby clst, Cmd.none)


    (StartGame, ClientLobby clst) ->
        -- this is only something master can do
        case clst.masterConfigState of
            Nothing -> (ClientLobby clst, Cmd.none)
            Just mcs ->
                -- make sure we have at least 1 player
                case clst.playerIds of
                    [] -> (ClientLobby clst, Cmd.none)
                    _ ->
                        -- make sure player IDs are unique
                        let startState =
                                {startingGameState | playerOrder = clst.playerIds
                                , players = initPlayers startingPlayerState clst.playerIds
                                , gameId = clst.gameId}
                                
                                -- if the rng is not filled in,
                                -- InitRandomAndDeal will take care of it                                
                            startStateRng = case mcs.rngInput of
                                                Nothing -> startState
                                                Just seed -> {startState | rng = seed}
                        in
                            (ClientPlayMaster { gameId = clst.gameId
                                              , gameState = startStateRng
                                              , message = clst.message
                                              , localPlayerIds = Set.fromList clst.localPlayerIds
                                              }
                            , Random.generate FinishStartingGame (Random.int Random.minInt Random.maxInt))
                
{-                       
    (SpectateGame, ClientPreGame cst) ->
        case cst.gidInput of
            Just gid ->
                let output =
                        succeed <| Encode.encode 0 <|
                            clientToServerMsgEncoder <|
                                SpectateGameCMsg gid
                in
                ( ClientSpectate { gameId = gid
                                         , gameState = Nothing
                                         , message = cst.message }
                , Task.perform (\x -> NoOp) SendToServer output
                )
            Nothing -> (state, Cmd.none)
-}

    {- TODO: let server/other clients know we quit? -}
    (RestartClient, _) ->
        (startingClientState, pushMsg QuitGameCMsg) -- send a message to the server

    {- Checks to see if the RNG default value (bogus value) has not been
       clobbered yet. If it is still default we use the random one we just generated -}
    {- I am concerned with how this interacts with logging -}
    (FinishStartingGame newRng, ClientPlayMaster cp) ->
        let cp' =
                if cp.gameState.rng == rngBogusValue
                then
                    {cp | gameState =
                         let cpGs = cp.gameState in
                         {cpGs | rng = newRng}}
                else
                    cp
        in
            cp' |> (\cp -> cp.gameState)
                |> initialDeal |> updateAndPushMasterState state

    -- if we are master, do the move locally, and then push update
    (SubmitMove pd, ClientPlayMaster cp) ->
        -- use tasks here
        let currentPlayer = dflHead cp.gameState.playerOrder dummyId in
        (ClientPlayMaster cp, sendInternalMsg (DoMove {play = pd, playerId = currentPlayer}))

    -- if not we push our move to the server
    (SubmitMove pd, ClientPlaySub cps) ->
        -- if we don't have a game state (from server) yet, do nothing
        -- (we could also keep track of players locally, but unnecessary i think)
        
        let currentPlayer = dflHead cps.gameState.playerOrder dummyId in
        (ClientPlaySub cps, pushMsg (MakeMoveCMsg cps.gameId {play = pd, playerId = currentPlayer}))

    (DoMove md, ClientPlayMaster cp) ->
        case md.play of
            PlayCard pos ->
                let
                    gst = cp.gameState
                    pId = dflHead gst.playerOrder dummyId
                    player = dflGet pId gst.players dummy
                    cId = dflNth pos player.hand urId
                    card = dflGet cId allCards urCard
                    newHand = dflDropNth pos player.hand
                in
                    case gst.phase of
                        ActionPhase ->
                            case card.playedEffect of
                                {- if this card is not an action, do not play it -}
                                Nothing -> (state, Cmd.none)
                                Just eff ->
                                    if gst.actions <= 0
                                    then (state, Cmd.none)
                                    {- calculate the state after deducting card costs -}
                                    else let gst' =
                                                 {gst | plays = cId :: gst.plays
                                                      , actions = gst.actions - 1
                                                      , players =
                                                          update' pId (\p -> {p | hand = newHand })
                                                              gst.players
                                                 }
                                         in gst' |> updateAndPushMasterState state
                      
                        CoinPhase ->
                            {- make sure we don't play valueless cards as coins -}
                            if card.spentValue <= 0 then (state, Cmd.none)
                            else
                                let 
                                    gst' =  {gst | plays = cId :: gst.plays
                                            , coin =  gst.coin + card.spentValue
                                            , players = update' pId (\p -> {p | hand = newHand})
                                                        gst.players
                                            }
                     
                                in gst' |> updateAndPushMasterState state

                        {- not the right time to play a card -}
                        _ -> (state, Cmd.none)

            BuyCard cId ->
                let
                    gst = cp.gameState
                    pId = dflHead gst.playerOrder dummyId
                    player = dflGet pId gst.players dummy
                    card = dflGet cId allCards urCard
                in
                    case gst.phase of
                        BuyPhase ->
                            if gst.buys > 0 && gst.coin >= card.cost
                            then
                                let gst' =
                                        { gst | purchases = cId :: gst.purchases
                                        , buys = gst.buys - 1
                                        , coin = gst.coin - card.cost
                                        , shop = update' cId (\i -> i - 1) gst.shop
                                        }
                                in
                                    if gameOver gst'
                                    then
                                        {- make sure the card they bought makes it into their deck
                                           if game is about to end -}
                                        let gst'' = {gst' | phase = EndGamePhase
                                                          , players =
                                                              update' pId
                                                                  (\p -> {p | discard = p.discard ++
                                                                                        p.hand ++
                                                                                        gst'.purchases ++
                                                                                        gst'.plays
                                                                            , hand = []}) gst'.players
                                                    }
                                        in gst'' |> updateAndPushMasterState state

                                    else gst' |> updateAndPushMasterState state

                            else (state, Cmd.none)

                        _ -> (state, Cmd.none)

            EndPhase ->
                let
                    gst = cp.gameState
                    pId = dflHead gst.playerOrder dummyId
                    player = dflGet pId gst.players dummy
                in
                    case gst.phase of
                        ActionPhase -> {gst | phase = CoinPhase} |> updateAndPushMasterState state
                        CoinPhase -> {gst | phase = BuyPhase} |> updateAndPushMasterState state
                        BuyPhase ->
                            {- first, add buys and plays back into discard pile, and reset resource values -}
                            let gst' = {gst | players =
                                                update' pId
                                                    (\p -> {p | discard = p.discard ++ p.hand ++
                                                                          gst.purchases ++ gst.plays
                                                              , hand = []}) gst.players
                                            , purchases = []
                                            , plays = []
                                            , coin = 0
                                            , buys = 1
                                            , actions = 1
                                            , phase = ActionPhase
                                       }
                            in
                                gst' |> dealPlayerCards pId 5 |> rotatePlayers |> updateAndPushMasterState state

                        _ -> (state, Cmd.none)

            -- for now, these are unused.
            Accept -> (state, Cmd.none)
            Decline -> (state, Cmd.none)

                
    -- handle asynchronous server messages
    (GotServerMsg msg, _) ->
        -- see if we are getting them this far
        let decodedResult = parseServerToClientMessage msg in
        case decodedResult of
            Ok decoded ->
                case decoded of
                    PlayerJoinedSMsg gId pIds ->
                        case state of
                            ClientLobby cls ->
                                -- only if we are master
                                case cls.masterConfigState of
                                    Nothing ->
                                        (state, Cmd.none)
                                    Just _ ->
                                        let playerIds' = cls.playerIds ++ pIds in
                                        -- TODO make sure we don't have duplicate players
                                        -- for now the server should check this
                                        (ClientLobby {cls | playerIds = playerIds'}
                                        -- send a "push lobby" command
                                        , pushMsg (MasterLobbyPushCMsg gId playerIds'))

                            _ -> (state, Cmd.none)
                                        
                    
                    UpdateGameStateSMsg gi gs msg ->
                        case state of
                            ClientLobby cls ->
                                -- lobby means the game just started
                                case cls.masterConfigState of
                                    Nothing ->
                                        -- make sure the ID is correct
                                        if cls.gameId == gs.gameId
                                        then (ClientPlaySub { gameId = cls.gameId
                                                            , gameState = gs
                                                            , message = Just msg
                                                            , localPlayerIds = Set.fromList cls.localPlayerIds}
                                             , Cmd.none)
                                        else (state, Cmd.none)

                                    -- only do this if we are not master
                                    Just mcs ->
                                        (state, Cmd.none)
                                            
                            ClientPlaySub cps ->
                                let newGameState =
                                        -- check to make sure the game ID is correct
                                        if cps.gameId == gs.gameId
                                        then gs
                                        else cps.gameState
                                in
                                (ClientPlaySub {cps | gameState = newGameState
                                                     , message = Just msg
                                               }, Cmd.none)

                            ClientPreGame cpgs -> (ClientPreGame {cpgs | message = Just msg}, Cmd.none)
                            ClientPlayMaster cpm -> (ClientPlayMaster {cpm | message = Just msg}, Cmd.none)

                    
                    MadeMoveSMsg gi md ->
                        case state of
                            -- TODO: check for game ID consistency?
                            ClientPlayMaster cpms ->
                                -- make sure it is that player's turn
                                -- TODO: do somthing to prevent spoofing
                                if List.head cpms.gameState.playerOrder == Just md.playerId
                                then (state, sendInternalMsg (DoMove md))
                                else (state, Debug.log "WRONG PLAYER" Cmd.none)

                            _ -> (state, Cmd.none)

                    -- if someone else quit, go back to pregame
                    PlayerQuitSMsg ->
                        (startingClientState, Cmd.none)

                    UpdateLobbySMsg players ->
                        case state of
                            ClientLobby cls ->
                                (ClientLobby {cls | playerIds = players}, Cmd.none)
                            
                            _ -> (state, Cmd.none)

                    LobbyResponseSMsg lobbyResp->
                        case state of
                            ClientPreGame cpst ->
                                case cpst.gidInput of
                                    Just gid ->
                                        -- TODO validate inputs, make sure they are valid (just gameId)
                                        -- maybe have another state type?
                                        let allPidsInput = case cpst.newPidInput of
                                                               Nothing -> cpst.pidsInput
                                                               Just p ->
                                                                   if p /= ""
                                                                   then if List.member p cpst.pidsInput
                                                                        then cpst.pidsInput
                                                                        else cpst.pidsInput ++ [p]
                                                                   else cpst.pidsInput
                                            clst =
                                                { gameId = gid
                                                , playerIds = allPidsInput
                                                , masterConfigState = Nothing
                                                , serverResponded = True
                                                , localPlayerIds = allPidsInput
                                                , message = cpst.message
                                                }
                                        in
                                            case lobbyResp of
                                                LobbyMaster ->
                                                    let masterConf = Just {rngInput = Nothing} in
                                                    (ClientLobby {clst | masterConfigState = masterConf}, Cmd.none)
                                                LobbyClient ->
                                                    (ClientLobby clst, Cmd.none)
                                                LobbyFail ->
                                                    (ClientPreGame
                                                         {cpst | message = Just "unable to join"}, Cmd.none)
                                    -- we need a game ID
                                    Nothing -> (state, Cmd.none)
                                
                            _ -> (state, Cmd.none)

                    -- is this a message type we don't understand?
                    _ -> (state, Cmd.none)
                         
            -- did we fail to even parse it?
            _ -> (state, Cmd.none)
        
    _ -> (state, Cmd.none)

subscriptions : ClientState -> Sub Msg
subscriptions model =
  -- takes a string
  fromServer GotServerMsg
