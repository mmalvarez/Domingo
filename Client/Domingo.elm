import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(..)
import Html.App
import Html.Events exposing (onClick)
import Dict
import List
import Random
import Task exposing (succeed)

{- import the rest of Domingo -}
import DomingoModel exposing (..)
import DomingoPorts exposing(..)
import DomingoActions exposing(..)
import DomingoCards exposing(..)

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

{- state of game at the beginning. for now we allow just 1 player with id 0
   this will be configurable to a greater degree later
   NB always update the RNG seed!
 -}
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
  , rng = 0 -- this _should_ always get overwritten
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
    , view = view
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
displayShop shop =
  table []
        (List.foldl (\cId acc -> acc ++ [displayCard cId (buttonGenShop cId shop), td [] []])
         [] (Dict.keys shop))

{- TODO: new we need a new view method -}
view clientState =
  let output = 
    case clientState of
      ClientPreGame _ ->
        div []
        [text "You have not started a game yet!"]

      _ -> text "not implemented yet"
  in
  div []
    ([output] ++
      [ br [] []
      , text "Created By Ronald X Hackerino"])

{- this used to be view; now it displays a game state -}
{-
view state =
  {- main div -}
  let
    currPid = dflHead state.playerOrder dummyId
    currPlayer = dflGet currPid state.players dummy
  in
  div []
    [ text ("Most recent server message: " ++ state.message), br [] []
    {-
    , div [] (if not (state.phase == EndGamePhase)
              then
                 [text ("You are player " ++ toString currPid ++ ". Your hand:")
                 , br [] []
                 , displayHand currPlayer.hand]
              else
                [ text "Game Over! Here are scores as (Id, score) pairs "
                , br [] []
                , text (toString state.winners) ])
    -}
    , br [] []
    , text "Your resources:", br [] []
    , text ("Actions: " ++ toString state.actions), br [] []
    , text ("Coin: " ++ toString state.coin), br [] []
    , text ("Buys: " ++ toString state.buys), br [] []
    , br [] []
    , displayShop state.shop
    , br [] []
    , br [] []
    , text ("Current Phase: " ++ toString state.phase)
    , br [] []
    , text ("Your deck has " ++ toString (List.length currPlayer.deck) ++ " cards")
    , br [] []
    , text (toString (List.length (state.plays)) ++ " cards have been played this turn; "
                  ++ (toString (List.length (state.purchases)) ++ " cards have been bought this turn"))
    , br [] []
    , button [ onClick EndPhase ] [ text "end current phase/turn"]
    , br [] []
    , button [ onClick StartGame ] [ text "start game" ]
    , button [ onClick SpectateGame ] [ text "spectate game" ]
    , text "with ID: ", input [ placeholder "Game ID", Html.Events.onInput UpdateGameId ] []
    , br [] []
    , button [ onClick QuitGame ]  [ text "quit game" ]
    , br [] [], br [] []
    , button [ onClick ShowState ] [ text "show game state in console" ]
    , br [] [], br [] [], br [] []

    ]
-}

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

    (StartGame, ClientPreGame cst) ->
        -- we can only start if inputs are filled in
        case (cst.gidInput, cst.rngInput) of
            (Just gid, Just rngIn) ->
                ( ClientPlay { gameId = gid
                             , gameState = {startingGameState | rng = rngIn}
                             , message = cst.message },
                  Random.generate InitRandom (Random.int Random.minInt Random.maxInt))
            (_, _) -> (state, Cmd.none)
                   
    (SpectateGame, ClientPreGame cst) ->
        case cst.gidInput of
            Just gid -> ( ClientSpectate { gameId = gid
                                         , gameState = Nothing
                                         , message = cst.message }, Cmd.none)
            Nothing -> (state, Cmd.none)

    {- TODO: let server/other clients know we quit? -}
    (QuitGame, _) ->
         case state of
             ClientPlay _ -> (startingClientState, Cmd.none)

             ClientSpectate _ -> (startingClientState, Cmd.none)

             _ -> (state, Cmd.none)

    {- Should now be called only when user requests it -}
    {- TODO: make sure we trigger the initial deal some other way -}
    (InitRandom newRng, ClientPreGame cpg) ->
        (ClientPreGame {cpg | rngInput = Just newRng}, Cmd.none)

            {-
        let gst = cp.gameState in
        case gst.phase of
            PreGamePhase ->
                let gst' = {gst | rng = newRng} in
                (ClientPlay { cp | gameState = gst'}
                            , doGameTask (initialDeal state'))
            
            _ -> (state, Cmd.none)
-}

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
