import Html exposing (div, button, text, br, img, table, tr, td, strong, h1, h2, h3, h4, h5, h6)
import Html.Attributes exposing (style)
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

{- state of game at the beginning. for now we allow just 1 player with id 0 -}
startingState : GameState
startingState =
  { players = Dict.fromList [(0, startingPlayerState), (1, startingPlayerState)]
  , playerOrder = [0,1]
  , shop = Dict.fromList [(estateId, estateHowMany), (copperId, copperHowMany),
                          (woodcutterId, woodcutterHowMany), (villageId, villageHowMany)]
  , trash = []
  , actions = 0, coin = 0, buys = 0
  , purchases = [], plays = []
  , phase = PreGamePhase
  , rng = 0 {- this is a dead value; gets clobbered by real randomness -}
  , winners = []
  , message = "nothing yet"
  }

main =
  Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
  }

init : (GameState, Cmd Msg)
init = (startingState, Cmd.none)

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

view state =
  {- main div -}
  let
    currPid = dflHead state.playerOrder dummyId
    currPlayer = dflGet currPid state.players dummy
  in
  div []
    [ text ("Most recent server message: " ++ state.message), br [] []
    , div [] (if not (state.phase == EndGamePhase)
              then
                 [text ("You are player " ++ toString currPid ++ ". Your hand:")
                 , br [] []
                 , displayHand currPlayer.hand]
              else
                [ text "Game Over! Here are scores as (Id, score) pairs "
                , br [] []
                , text (toString state.winners) ])
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
    , br [] []
    , button [ onClick RestartGame ]  [ text "restart game" ]
    , br [] [], br [] []
    , Html.form [Html.Attributes.id "connectForm"]
         [Html.input [ Html.Attributes.id "connectInput"
         , Html.Attributes.type' "submit"
         , Html.Attributes.value "Submit"
         ] []]
    , br [] []
    , button [ onClick ShowState ] [ text "show game state in console" ]
    , br [] [], br [] [], br [] []
    , text "Created By Ronald X Hackerino"
    ]

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
tallyScores : GameState -> GameTask
tallyScores state =
  let
    scorers = Dict.foldl
                (\k v acc -> ((v.victory + scoreCards (v.deck ++ v.hand ++ v.discard)), k) :: acc)
                [] state.players

    winners = List.sortWith (\(s1,p1) (s2,p2) -> compare s2 s1) scorers

  in succeed {state | winners = List.map (\(s,p) -> (p,s)) winners}

{- Respond to messages -}
update : Msg -> GameState -> (GameState, Cmd Msg)
update msg state =
  case msg of

    ShowState ->
      (Debug.log (toString state) state, Cmd.none)

    StartGame ->
      case state.phase of
        PreGamePhase ->
          ({ state | phase = DealPhase},
           Random.generate InitRandom (Random.int Random.minInt Random.maxInt))

        _ -> (state, Cmd.none)

    RestartGame ->
      (startingState, Cmd.none)

    InitRandom newRng ->
      let state' = {state | rng = newRng} in
      (state', doGameTask (initialDeal state'))

    UpdateState state' ->
      (state', Cmd.none)

    PlayCard pos ->
      let foo = Debug.log ("Playing card in position: " ++ toString pos) "" in
      let
        pId = dflHead state.playerOrder urId

        player = dflGet pId state.players dummy

        cId = dflNth pos player.hand -1

        card = dflGet cId allCards urCard

        newHand = dflDropNth pos player.hand
      in
      case state.phase of
        ActionPhase ->
          {- if this card is not an action -}
          if card.playedEffect == Nothing
          then (state, Cmd.none)
          else
            if state.actions > 0
            {- calculate the state after deducting card costs -}
            then let state' =
              {state | plays = cId :: state.plays
                     , actions = state.actions - 1
                     , players = update' pId (\p -> {p | hand = newHand })
                                  state.players
              } in
              case card.playedEffect of
                Nothing -> (state', Cmd.none)
                Just effect -> effect state'

            else (state, Cmd.none)

        CoinPhase ->
          {- make sure we don't play valueless cards as coins -}
          if card.spentValue > 0 then
            ( {state | plays = cId :: state.plays
                     , coin =  state.coin + card.spentValue
                     , players = update' pId (\p -> {p | hand = newHand}) state.players
              }
            , Cmd.none)
          else (state, Cmd.none)

        _ -> (state, Cmd.none)

    BuyCard cId ->
      let foo = Debug.log ("Buying card with id: " ++ toString cId) "" in
      let
        pId = dflHead state.playerOrder urId

        player = dflGet pId state.players dummy

        card = dflGet cId allCards urCard
      in
      case state.phase of
        BuyPhase ->
          if state.buys > 0 && state.coin >= card.cost
          then
            let state' =
              { state | purchases = cId :: state.purchases
                      , buys = state.buys - 1
                      , coin = state.coin - card.cost
                      , shop = update' cId (\i -> i - 1) state.shop
               }
            in
            if gameOver state'
            then
              {- make sure the card they bought makes it into their deck -}
              let state'' = {state' | phase = EndGamePhase
                                    , players = update' pId
                                                  (\p -> {p | discard = p.discard ++ p.hand ++ state'.purchases ++ state'.plays
                                                            , hand = []
                                                  }) state'.players
                            } in
              (state'', doGameTask (tallyScores state''))

            else (state', Cmd.none)

          else (state, Cmd.none)

        _ -> (state, Cmd.none)

    EndPhase ->
      let
        pId = dflHead state.playerOrder urId

        player = dflGet pId state.players dummy
      in
      case state.phase of
        ActionPhase -> ({state | phase = CoinPhase}, Cmd.none)
        CoinPhase -> ({state | phase = BuyPhase}, Cmd.none)
        BuyPhase ->
          {- first, add buys and plays back into discard pile, and reset resource values -}
          let state' = {state | players = update' pId
                                            (\p -> {p | discard = p.discard ++ p.hand ++ state.purchases ++ state.plays
                                                      , hand = []
                                                   }
                                            ) state.players
                              , purchases = []
                              , plays = []
                              , coin = 0
                              , buys = 1
                              , actions = 1
                              , phase = ActionPhase
                       }
          in
          (state', doGameTask (Task.andThen (dealPlayerCards pId 5 state')
                                (\st -> rotatePlayers st)))

        _ -> (state, Cmd.none)

    GotServerMsg msg ->
      (state, Cmd.none)

    _ -> (state, Cmd.none)

subscriptions : GameState -> Sub Msg
subscriptions model =
  -- takes a string
  fromServer GotServerMsg

