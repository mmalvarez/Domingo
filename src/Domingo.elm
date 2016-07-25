import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(..)
import Html.App
import Dict
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
  , rngInput = Nothing
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

sendStateToServer : GameState -> Cmd Msg
sendStateToServer gs =
    let
        output =
            succeed <| Encode.encode 0 <|
                clientToServerMsgEncoder <|
                    UpdateGameCMsg gs.gameId gs
    in
    Task.perform
        (\x -> NoOp)
        SendToServer output
                  
{- Convenience wrapper for mucking with client's game state -}
-- TODO have a pure sub-function for others to call?
updateGameState : ClientState -> GameState -> (ClientState, Cmd Msg)
updateGameState cs gs =
    case cs of
        {- note that we overwrite game ID to keep things consistent -}
        ClientPlay cps -> ( ClientPlay {cps | gameId = gs.gameId
                                          , gameState = gs}
                          -- the following will cause
                          -- game state to be sent over the wire
                          , sendStateToServer gs
                          )

        ClientSpectate css -> ( ClientSpectate {css | gameId = gs.gameId
                                                   , gameState = Just gs}
                              , Cmd.none
                              )

        _ -> (cs, Cmd.none)
      
{- Respond to messages -}
update : Msg -> ClientState -> (ClientState, Cmd Msg)
update msg state =
  case (msg, state) of

    (ShowState, _) ->
      (Debug.log (toString state) state, Cmd.none)

    (UpdateGameId gId, ClientPreGame cst) ->
        (ClientPreGame {cst | gidInput = Just gId}, Cmd.none)

    -- no-op if
    -- TODO this will cause issues with text box contents being out of sync
    -- let's fix this later
    (UpdateSeedToUse seedStr, ClientPreGame cst) ->
        case (toInt seedStr) of
            Ok seed -> (ClientPreGame {cst | rngInput = Just seed}, Cmd.none)
            Err _ -> (ClientPreGame cst, Cmd.none)

    -- TODO make this send a message to server
    (StartGame, ClientPreGame cst) ->
        -- we can only start if game id is filled in
        -- if the rng is not filled in,
        -- InitRandomAndDeal will take care of it                              
        case cst.gidInput of
            Just gid ->
                let startState =
                        case cst.rngInput of
                            Just rngIn ->
                                {startingGameState | rng = rngIn
                                                   , gameId = gid
                                }
                            Nothing ->
                                {startingGameState | gameId = gid}
                in
                ( ClientPlay
                       { gameId = gid
                       , gameState = startState
                       , message = cst.message
                       },
                  Random.generate FinishStartingGame
                      (Random.int Random.minInt Random.maxInt))
                    
            Nothing -> (state, Cmd.none)

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

    {- TODO: let server/other clients know we quit? -}
    (RestartClient, _) ->
        (startingClientState, Cmd.none)

    {- Checks to see if the RNG default value (bogus value) has not been
       clobbered yet. If it is still default we use the random one we just generated -}
    {- I am concerned with how this interacts with logging -}
    (FinishStartingGame newRng, ClientPlay cp) ->
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
                |> initialDeal |>
                   updateGameState (ClientPlay cp')
            
  {- TODO: Make sure all state updates go through a single
       function/message to make sure all get logged for spectator mode
     -}
    (UpdateClientState cst, _) ->
      (cst, Cmd.none)

    (PlayCard pos, ClientPlay cp) ->
      let foo = Debug.log ("Playing card in position: " ++ toString pos) "" in
      let
        gst = cp.gameState
          
        pId = dflHead gst.playerOrder urId

        player = dflGet pId gst.players dummy

        cId = dflNth pos player.hand -1

        card = dflGet cId allCards urCard

        newHand = dflDropNth pos player.hand

      in

      case gst.phase of
        ActionPhase ->
         case card.playedEffect of
             {- if this card is not an action, do not play it -}
             Nothing -> (state, Cmd.none)

             Just eff ->
                if gst.actions > 0
                  {- calculate the state after deducting card costs -}
                  then let gst' =
                      {gst | plays = cId :: gst.plays
                      , actions = gst.actions - 1
                      , players = update' pId (\p -> {p | hand = newHand })
                                  gst.players}
                  in
                      -- TODO I am concerned this has the wrong precedence (elsewhere too)
                      gst' |> updateGameState state

                  else (state, Cmd.none)
                      
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
                     
                in gst' |> updateGameState state
                    
        {- not the right time to play a card -}
        _ -> (state, Cmd.none)
            
    (BuyCard cId, ClientPlay cp) ->
      let foo = Debug.log ("Buying card with id: " ++ toString cId) "" in
      let
        gst = cp.gameState
          
        pId = dflHead gst.playerOrder urId

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
              {- make sure the card they bought makes it into their deck if game is about to end -}
              let gst'' = {gst' | phase = EndGamePhase
                                , players =
                                    update' pId
                                        (\p -> {p | discard = p.discard ++
                                                              p.hand ++
                                                              gst'.purchases ++
                                                              gst'.plays
                                                  , hand = []})
                                         gst'.players
                            } in
              {- tallyScores -} gst'' |> updateGameState state

            else gst' |> updateGameState state

          else (state, Cmd.none)

        _ -> (state, Cmd.none)

    (EndPhase, ClientPlay cp) ->
      let
        gst = cp.gameState
        pId = dflHead gst.playerOrder urId
        player = dflGet pId gst.players dummy
      in
      case gst.phase of
        ActionPhase -> {gst | phase = CoinPhase} |> updateGameState state
        CoinPhase -> {gst | phase = BuyPhase} |> updateGameState state
        BuyPhase ->
          {- first, add buys and plays back into discard pile, and reset resource values -}
          let gst' = {gst     | players = update' pId
                                            (\p -> {p | discard = p.discard ++ p.hand ++ gst.purchases ++ gst.plays
                                                      , hand = []
                                                   }
                                            ) gst.players
                              , purchases = []
                              , plays = []
                              , coin = 0
                              , buys = 1
                              , actions = 1
                              , phase = ActionPhase
                       }
          in
          gst' |> dealPlayerCards pId 5 |> rotatePlayers |> updateGameState state

        _ -> (state, Cmd.none)

    -- handle asynchronous server messages
    (GotServerMsg msg, _) ->
        -- see if we are getting them this far
        let decodedResult = Debug.log "got here!!!" <| parseServerToClientMessage msg in
        case decodedResult of
            Ok decoded ->
                case decoded of
                    UpdateGameSMsg gs msg ->
                        -- TODO add a way to acknowledge you got message
                        case state of
                            -- only perform the update if we are a spectator
                            ClientSpectate css ->
                                let newGameState =
                                        -- check to make sure the game ID is correct
                                        if css.gameId == gs.gameId
                                        then Just gs
                                        else css.gameState
                                in
                                ((ClientSpectate {css | gameState = newGameState
                                                      , message = Just msg
                                                 }), Cmd.none) 

                            ClientPreGame cpgs -> (ClientPreGame {cpgs | message = Just msg}, Cmd.none)
                            ClientPlay cps -> (ClientPlay {cps | message = Just msg}, Cmd.none)

                    -- is this a message type we don't understand?
                    _ -> (state, Cmd.none)
            -- did we fail to even parse it?
            _ -> (state, Cmd.none)
        
     -- send a message to the server
     -- this should do a real command!
    (SendToServer str, state) ->
        (state, toServer str)

    _ -> (state, Cmd.none)

subscriptions : ClientState -> Sub Msg
subscriptions model =
  -- takes a string
  fromServer GotServerMsg
