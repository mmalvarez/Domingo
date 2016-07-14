import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(..)
import Html.App
import Dict
import List
import Random
import Task exposing (succeed)
import String exposing (toInt)

{- import the rest of Domingo -}
import DomingoModel exposing (..)
import DomingoPorts exposing(..)
import DomingoActions exposing(..)
import DomingoCards exposing(..)


{- TODO: Fix div structure of display functions -}

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
  , winners = []
  , message = "" -- TODO eventually get rid of this?
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
        ([ strong [] [h3 [] [text c.name]], tr [] []
         , strong [] [text c.kind], tr [] []
         , text ("Victory: " ++ toString c.victory), tr [] []
         , text ("Value: " ++ toString c.spentValue), tr [] []
         , text ("Cost: " ++ toString c.cost) ] ++
           footer)

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
displayShop shop isSpectator =
  table []
        (List.foldl (\cId acc -> acc ++
                         [displayCard cId (buttonGenShop cId shop), td [] []])
         [] (Dict.keys shop))

-- display the state of the game
-- isSpectator isn't used right now, may be later?
displayGameState gameState isSpectator =
    let
        currPid = dflHead gameState.playerOrder dummyId
        currPlayer = dflGet currPid gameState.players dummy
        output =
            if gameState.phase == EndGamePhase
              then
                  -- display score if game is over
                  [ text "Game Over! Here are scores as (Id, score) pairs "
                  , br [] []
                  , text (toString gameState.winners) ]

              else
                  -- otherwise, show the game
                  [text ("You are player " ++ toString currPid ++ ". Your hand:")
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
    in div [] output
        
                              
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
                       [displayGameState cpState.gameState False]
  
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
                
            Nothing -> ""
            Just m -> m
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

{- called after game ends, determine winner -}
tallyScores : GameState -> GameState
tallyScores state =
  let
    scorers = Dict.foldl
                (\k v acc -> ((v.victory + scoreCards (v.deck ++ v.hand ++ v.discard)), k) :: acc)
                [] state.players

    winners = List.sortWith (\(s1,p1) (s2,p2) -> compare s2 s1) scorers

  in {state | winners = List.map (\(s,p) -> (p,s)) winners}

{- Convenience wrapper for mucking with client's game state -}
updateGameState : ClientState -> GameState -> ClientState
updateGameState cs gs =
    case cs of
        {- note that we overwrite game ID to keep things consistent -}
        ClientPlay cps -> ClientPlay {cps | gameId = gs.gameId
                                          , gameState = gs}

        ClientSpectate css -> ClientSpectate {css | gameId = gs.gameId
                                                  , gameState = Just gs}

        _ -> cs
      
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

    (StartGame, ClientPreGame cst) ->
        -- we can only start if game id is filled in
        -- if the rng is not filled in,
        -- InitRandomAndDeal will take care of it                                
        case cst.gidInput of
            Just gid ->
                let startState =
                        case cst.rngInput of
                            Just rngIn ->
                                {startingGameState | rng = rngIn}
                            Nothing ->
                                startingGameState
                in
                ( ClientPlay
                       { gameId = gid
                       , gameState = startingGameState
                       , message = cst.message
                       },
                  Random.generate InitRandomAndDeal
                      (Random.int Random.minInt Random.maxInt))
                    
            Nothing -> (state, Cmd.none)
                   
    (SpectateGame, ClientPreGame cst) ->
        case cst.gidInput of
            Just gid -> ( ClientSpectate { gameId = gid
                                         , gameState = Nothing
                                         , message = cst.message }, Cmd.none)
            Nothing -> (state, Cmd.none)

    {- TODO: let server/other clients know we quit? -}
    (RestartClient, _) ->
        (startingClientState, Cmd.none)

    {- Checks to see if the RNG default value (bogus value) has not been
       clobbered yet. If it is still default we use the random one we just generated -}
    (InitRandomAndDeal newRng, ClientPlay cp) ->
        let cp' =
                if cp.gameState.rng == rngBogusValue
                then
                    {cp | gameState =
                         let cpGs = cp.gameState in
                         {cpGs | rng = newRng}}
                else
                    cp
        in
            let cpUpdateGameState cp gSt =
                    {cp | gameState = gSt}
            in
                (cp' |> (\cp -> cp.gameState)
                      |> initialDeal |> updateGameState (ClientPlay cp'),
                          Cmd.none
                         )

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
                      (gst' |> updateGameState state
                      , Cmd.none)

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
                     
                in (gst' |> updateGameState state, Cmd.none)
                    
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
              (tallyScores gst'' |> updateGameState state, Cmd.none)

            else (gst' |> updateGameState state, Cmd.none)

          else (state, Cmd.none)

        _ -> (state, Cmd.none)

    (EndPhase, ClientPlay cp) ->
      let
        gst = cp.gameState
        pId = dflHead gst.playerOrder urId
        player = dflGet pId gst.players dummy
      in
      case gst.phase of
        ActionPhase -> ({gst | phase = CoinPhase} |> updateGameState state, Cmd.none)
        CoinPhase -> ({gst | phase = BuyPhase} |> updateGameState state, Cmd.none)
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
          (gst' |> dealPlayerCards pId 5 |> rotatePlayers |> updateGameState state, Cmd.none)

        _ -> (state, Cmd.none)

    -- handle asynchronous server messages
    (GotServerMsg msg, ClientPreGame cpgs) ->
        (ClientPreGame {cpgs | message = Just msg}, Cmd.none)

    (GotServerMsg msg, ClientPlay cps) ->
        (ClientPlay {cps | message = Just msg}, Cmd.none)

    (GotServerMsg msg, ClientSpectate css) ->
        (ClientSpectate {css | message = Just msg}, Cmd.none)

-- not implementd yet
--    (GotServerMsg msg, ClientSpectate cs) ->
--      ({gst | message})

    _ -> (state, Cmd.none)

subscriptions : ClientState -> Sub Msg
subscriptions model =
  -- takes a string
  fromServer GotServerMsg
