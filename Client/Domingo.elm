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
import DomingoModel exposing (..)
import DomingoPorts exposing(..)
import DomingoActions exposing(..)
import DomingoCards exposing(..)


{- TODO: Fix div structure of display functions -}
{- TODO: implement initial deal more sanely,
   use updateState -}
{- TODO: More logically divide between Domingo,
   DomingoActions, and DomingoConfig -}

{- state of a player at the beginning -}
startingDeck : List CardId
startingDeck = List.repeat 7 copperId ++ List.repeat 3 estateId

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
  { players = Dict.fromList [(0, startingPlayerState), (1, startingPlayerState)]
  , playerOrder = [0,1]
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

startingClientState : ClientState
startingClientState = ClientPreGame
  { gidInput = Nothing
  , rngInput = Nothing
  , message = Nothing
  }

main =
  Html.App.program
    { init = init
    , view = displayClientState
    , update = update
    , subscriptions = subscriptions
  }

init : (ClientState, Cmd Msg)
init = (startingClientState, Cmd.none)

{- button generators -}

 {- for buttons on hand cards -}
buttonGenHand pos =
  [tr [] [], button [ onClick (PlayCard pos) ] [ text ({-(toString pos) ++ -}" play") ]]

{- for buttons on shop cards (TODO) -}

{- output a card as HTML -}
{- takes card ID, its position in the list, and additional stuff to put at the bottom -}
displayCard cId footer =
  let c = dflGet cId allCards urCard in
  table [Html.Attributes.style [("outlineColor", "black"), ("outlineStyle", "solid")]]
        ([ tr [] [ td [] [strong [] [h3 [] [text c.name]]] ]
         , tr [] [ td [] [strong [] [text c.kind]]]
         , tr [] [ td [] [text ("Victory: " ++ toString c.victory) ] ]
         , tr [] [ td [] [text ("Value: " ++ toString c.spentValue)] ]
         , tr [] [ td [] [text ("Cost: " ++ toString c.cost) ] ] ] ++
           footer) -- TODO should footer really be in the table?

{- bind for Maybe -}
(>>==) mx f =
  case mx of
    Just x -> f x
    Nothing -> Nothing

infixr 1 >>==

{- display your hand -}
{- outer div, then inner divs for each card. each card gets a name, description, and a play button -}
displayHand cards =
  table []
        (fst <| List.foldl (\cId (list, len) -> (list ++ [displayCard cId (buttonGenHand len), td [] []], len + 1))
                            ([], 0) cards)

{- button for shop -}
buttonGenShop cId shop =
  [tr [] [], button [ onClick (BuyCard cId)] [ text ("Buy (" ++ toString (dflGet cId shop 0) ++ " remain)") ]]

{- display cards in shop -}
-- isSpectator used to control whether buttons show up, eventually
-- TODO fix this table structure pls
displayShop shop isSpectator =
  table []
        (List.foldl (\cId acc -> acc ++
                         [td [] [displayCard cId (buttonGenShop cId shop)], td [] []])
         [] (Dict.keys shop))

displayWinnersTable gst =
  let
    scorers = Dict.foldl
                (\k v acc -> ((v.victory + scoreCards (v.deck ++ v.hand ++ v.discard)), k) :: acc)
                [] gst.players

    winners = List.sortWith (\(s1,p1) (s2,p2) -> compare s2 s1) scorers

  in List.map (\(s,p) ->
      tr [] [ td [] [text ("Player " ++ toString p)]
            , td [] [text (toString p ++ " VP")] ]
      ) winners

-- display the winners list for the given game state
displayWinners gst =
    div [] [ text "Game Over! Scores:", br [] []
           , table [] ( [ tr [] [ th [] [text "player"]
                                , th [] [text "victory score"]]]
                            ++ displayWinnersTable gst)]

-- display the state of the game
-- isSpectator isn't used right now, may be later?
displayGameState gameState isSpectator =
    let
        currPid = dflHead gameState.playerOrder dummyId
        currPlayer = dflGet currPid gameState.players dummy
        output =
            if gameState.phase == EndGamePhase
              then
                  displayWinners gameState
              else
                  -- otherwise, show the game
                  div [] [ text ("You are player " ++ toString currPid ++ ". Your hand:")
                         , br [] []
                         , displayHand currPlayer.hand
                         , br [] []
                         , text "Your resources:", br [] []
                         , text ("Actions: " ++ toString gameState.actions), br [] []
                         , text ("Coin: " ++ toString gameState.coin), br [] []
                         , text ("Buys: " ++ toString gameState.buys), br [] []
                         , br [] []
                         , text "Shop:", br [] []
                         , displayShop gameState.shop isSpectator
                         , br [] []
                         , br [] []
                         -- TODO prune/reorganize this information display
                         , text ("Current Phase: " ++ toString gameState.phase)
                         , br [] []
                         , text ("Your deck has " ++ toString (List.length currPlayer.deck) ++ " cards")
                         , br [] []
                         , text (toString (List.length (gameState.plays)) ++ " cards have been played this turn; "
                                     ++ (toString (List.length (gameState.purchases)) ++ " cards have been bought this turn"))
                         ]
    in output
        
                              
-- display the main div
displayMainDiv clientState =
    case clientState of
        ClientPreGame _ ->
            div []
                [ h1 [] [text "Domingo"]
                -- buttons
                , input [onInput UpdateGameId, placeholder "Game ID"] []
                , br [] []
                , input [onInput UpdateSeedToUse, placeholder "Optional Seed; Randomly Generated if Empty"] []
                , br [] []
                , button [onClick StartGame]  [text "start game"]
                , br [] []
                , button [onClick SpectateGame] [text "spectate game"]
                , br [] []
                , button [ onClick RestartClient ]  [ text "restart client" ]
                ]

        ClientPlay cpState ->
            div []
                <| [ h2 [] [text <| "Playing Domingo. Game ID: " ++ toString cpState.gameId] ]
                   ++
                   -- false because you are not spectator
                   [displayGameState cpState.gameState False] ++
                   [button [onClick EndPhase] [text "end phase/turn"]]
  
        ClientSpectate csState ->
            case csState.gameState of
                -- TODO - limit knowledge of game state (eventually...)
                Just gSt ->
                    -- true because you are spectator
                    displayGameState gSt True
                        
                Nothing ->
                    text "Your game has not started yet."


{- Displays some options that affect the entire
   client and are displayed regardless of game state -}
displayClientOptions clientState =
    let message =
            case
            (case clientState of
                 ClientPreGame cpg -> cpg.message
                 
                 ClientPlay cpg -> cpg.message
                                                           
                 ClientSpectate cpg -> cpg.message
            ) of
                
            Nothing -> "message:"
            Just m -> "message: " ++ m
    in
    div []
        [ button [ onClick RestartClient ] [text "restart client" ]
        , br [] []
        , text ("Most recent server message: " ++ message)
        , br [] []
        , button [ onClick ShowState ] [ text "show client state in console" ]
        -- TODO: eventually add a server maintenance button here?
        ]
            
displayClientState clientState =
  let mainDiv = displayMainDiv clientState
      clientOptions =  displayClientOptions clientState
  in
  div []
    ([ mainDiv ] ++
      [ br [] [], br [] []] ++
      [ clientOptions ] ++
      [ br [] [], br [] []
      , text "Created By Ronald X Hackerino"])

{- predicate checking for game over (called after each purchase) -}
gameOver : GameState -> Bool

{- game ends if there are no more estates -}
gameOver state =
  let numEstates = dflGet estateId state.shop 0 in
  if numEstates == 0 then True
    else False

{- sum how many victory points a list of cards is worth -}
scoreCards : List CardId -> Int
scoreCards l =
  List.foldl (\cId i ->
              let c = dflGet cId allCards urCard in
              c.victory + i) 0 l

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
{- TODO wrap this in something that deals with effects
   (namely, submitting game states to the server) -}
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
