import Html exposing (div, button, text, br, img, table, tr, td, strong, h1, h2, h3, h4, h5, h6)
import Html.Attributes exposing (style)
import Html.App
import Html.Events exposing (onClick)
import Dict
import List
import Random
import Task exposing (succeed)

type alias PlayerId = Int

type alias CardId = Int

type alias PlayerState =
  { deck : List CardId
  , discard : List CardId
  , hand : List CardId
  , victory : Int {- Extra VP earned from e.g. actions -}
  , valid : Bool
}

{- whose turn is it? is the game over? -}
{- TODO: eventually should have a pregame phase -}
{- use continuations to have "response" phases -}
type GamePhase = PreGamePhase
               | DealPhase
               | ActionPhase
               | CoinPhase
               | BuyPhase
               | EndGamePhase {- game is over -}

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
  , phase : GamePhase
  , rng : Int
  {- used at end of game to display how well people did in number of victory points -}
  , winners : List (PlayerId, Int)
}

type Msg = IncrementCoin | DecrementCoin
          {- for debugging -}
          | ShowState
          {- userful ones -}
          | StartGame
          | RestartGame
          | PlayCard Int
          | EndPhase
          | BuyCard CardId
          | EndBuy
          {- message sent when new random seed is generated -}
          | InitRandom Int
          {- if a task fails - should be a no op -}
          | TaskFail String
          {- update the game state -}
          | UpdateState GameState

type alias Card =
  { idn : CardId
  , name : String
  , img : Maybe String
  , text : Maybe String
  , kind : String {- TODO maybe make this semantic. is it action, victory, etc -}
  , victory : Int {- how much victory is this worth at end of game? -}
  , spentValue : Int {- how much is it worth if played as money? -}
  , cost : Int {- how much does it cost to buy from the shop? -}
  , playedEffect : Maybe (GameState -> (GameState, Cmd Msg)) {- what happens when you play it as an action -}
  {- to do: some way of specifying reactions when it's in your hand -}
  {- to do: some way of specifying reactions when it's on the field -}
}

{- ** Card Definitions ** -}

{- no-op action, for cards that don't have any effect -}
playerStateId : PlayerState -> PlayerState
playerStateId ps = ps

gameStateId : GameState -> GameState
gameStateId gs = gs

{- we build other cards from this one -}
urId = -1

urCard : Card
urCard =
  { idn = urId, name = "Ur-Card", img = Nothing, text = Nothing
  , kind = "", victory = 0, spentValue = 0, cost = Random.maxInt
  , playedEffect = Nothing }

{- TODO, perhaps have a hierarchy of these for each type of card -}

copperId = 0

copperCard : Card
copperCard =
  { urCard | idn = copperId
           , name = "Copper"
           , text = Just "Worth 3 Coin"
           , kind = "Money"
           , spentValue = 1
           , cost = 0
  }

estateId = 1000

estateCard : Card
estateCard =
  { urCard | idn = estateId
           , name = "Estate"
           , text = Just "Worth 1 Victory"
           , kind = "Victory"
           , cost = 2
           , victory = 1
  }

woodcutterId = 2000

woodcutterCard =
  { urCard | idn = woodcutterId
           , name = "Woodcutter"
           , text = Just "2 coin and 1 buy"
           , kind = "Action"
           , cost = 3
           , playedEffect = Just (\st -> ({st | buys = st.buys + 1
                                             , coin = st.coin + 2}, Cmd.none))
  }

villageId = 2001

villageCard =
  { urCard | idn = villageId
           , name = "Village"
           , text = Just "1 card and 2 actions"
           , kind = "Action"
           , cost = 3
           , playedEffect = Just (\st -> let st' = {st | actions = st.actions + 2} in
                                            (st',
                                             doGameTask (dealCurrentPlayerCards 1 st')))
  }

{- global dictionary used to look up cards by ID -}

allCards : Dict.Dict CardId Card
allCards = Dict.fromList
            [ (urId, urCard)
            , (copperId, copperCard)
            , (estateId, estateCard)
            , (woodcutterId, woodcutterCard)
            , (villageId, villageCard)
            ]

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

{- how to shuffle a deck. -}
{- Pick a single card from a deck. Should not be given [] -}
{- TODO see if this and shuffle can be made polymorphic -}
pickFromList : List CardId -> Random.Seed -> (List CardId, CardId, Random.Seed)
pickFromList l s =
  let
    (n, newSeed) = Random.step (Random.int 1 (List.length l)) s

    newList = List.take (n - 1) l ++ List.drop n l

    newCard = List.head (List.drop (n - 1) l)
  in
  case newCard of
    Nothing ->
      (newList, urId, newSeed) {- should be dead code -}

    Just nc ->
      (newList, nc, newSeed)

{- shuffles the list -}
shuffle' : List CardId -> Random.Seed -> (List CardId, Random.Seed)
shuffle' l s =
  case l of
      [] -> ([], s)
      _ ->
        let
          (l', c, s') = pickFromList l s

          (l'', s'') = shuffle' l' s'
        in
        (c :: l'', s'')

{- get back our RNG seed from the seed value -}
intOfSeed : Random.Seed -> Int
intOfSeed s =
  fst <| Random.step (Random.int Random.minInt Random.maxInt) s

{- nicer wrapper -}
shuffle : List CardId -> Int -> List CardId
shuffle l iSeed =
  fst <| shuffle' l (Random.initialSeed iSeed)

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
    [ div [] (if not (state.phase == EndGamePhase)
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
    , button [ onClick ShowState ] [ text "show game state in console" ]
    , br [] [], br [] [], br [] []
    , text "Created By Ronald X Hackerino"
    ]

{- variant of head that works with default values -}
dflHead : List a -> a -> a
dflHead l d = case l of
              h :: t -> h

              _ -> d

{- variant of tail that swallows errors -}
dflTail : List a -> List a
dflTail l =
  case l of
    [] -> []
    h :: t -> t

{- dummy player for use with the next function -}
dummyId : PlayerId
dummyId = -1

dummy : PlayerState
dummy =
  { deck = []
  , discard = []
  , hand = []
  , victory = 0
  , valid = False
  }

{- TODO use IDs below, not states -}

{- similar for get ? -}
dflGet : comparable -> Dict.Dict comparable b -> b -> b
dflGet p d default = case Dict.get p d of
                      Just r -> r
                      Nothing -> default

{- Task related things, for chaining together actions on a game state -}
type alias GameTask = Task.Task String GameState

doGameTask : GameTask -> Cmd Msg
doGameTask = Task.perform (\str -> TaskFail str)
                          (\state -> UpdateState state)

{- update'. this function updates a value in a Dict. it is a noop if key is not there -}
update' : comparable -> (v -> v) -> Dict.Dict comparable v -> Dict.Dict comparable v
update' k f d =
  case (Dict.get k d) of
    Just v -> Dict.insert k (f v) d
    Nothing -> d

{- this is a no-op unless player has an empty deck and nonempty discard pile -}
{- TODO maybe it would be better to have this shuffle your deck into your discard pile -}
shufflePlayerDeck : PlayerId -> GameState -> GameTask
shufflePlayerDeck pId st =
  let p = dflGet pId st.players dummy in
  if (not <| List.isEmpty p.deck) || (List.isEmpty p.deck && List.isEmpty p.discard)
    then
      succeed st
    else
      let (newDeck, newSeed) = shuffle' p.discard <| Random.initialSeed st.rng in
      succeed {st | rng = intOfSeed newSeed
                  , players = update' pId
                                (\p -> {p | deck = newDeck, discard = []})
                                st.players
              }

dealPlayerCard : PlayerId -> GameState -> GameTask
dealPlayerCard pId st =
  let p = dflGet pId st.players dummy in
  {- if there is a card to deal, deal it -}
  if (not <| List.isEmpty p.deck)
    then
      succeed {st | players = update' pId
                        (\p -> {p | deck = dflTail p.deck, hand = dflHead p.deck urId :: p.hand})
                        st.players
              }
  else
    if (not <| List.isEmpty p.discard)
      {- shuffle and retry if the discard isn't empty -}
      then
        (shufflePlayerDeck pId st) `Task.andThen` (\st -> dealPlayerCard pId st)
      {- otherwise, no-op -}
      else succeed st

dealPlayerCards : PlayerId -> Int -> GameState -> GameTask
dealPlayerCards p n st =
  if n == 0 then succeed st
  else
    dealPlayerCard p st `Task.andThen` (\st -> dealPlayerCards p (n-1) st)

{- deal a card to current player (at front of rotation) -}
dealCurrentPlayerCards : Int -> GameState -> GameTask
dealCurrentPlayerCards n st =
  let current = dflHead st.playerOrder -1 in
  dealPlayerCards current n st

rotatePlayers : GameState -> GameTask
rotatePlayers st =
  succeed ({st | playerOrder = List.drop 1 st.playerOrder ++ List.take 1 st.playerOrder})

initialDeal : GameState -> GameTask
initialDeal st =
    {- check if player 1 has a hand -}
    let
      nextId = (dflHead st.playerOrder -1)

      nextPlayer = dflGet nextId st.players dummy
    in
    if nextPlayer.valid then
      {- if so, we have dealt to everyone already -}
      if not (List.isEmpty nextPlayer.hand) then
        {- start the first action phase -}
        succeed {st | phase = ActionPhase
                    , actions = 1
                    , coin = 0
                    , buys = 1
                }

      else
        {- if not, deal them a hand -}
        Task.andThen (shufflePlayerDeck nextId st)
                     (\st -> Task.andThen (dealPlayerCards nextId 5 st)
                     (\st -> Task.andThen (rotatePlayers st)
                     (\st -> initialDeal st)))

    {- make sure we don't deal to dummy player, probably unnecessary -}
    else succeed st

{- get nth item in list -}
dflNth : Int -> List a -> a -> a
dflNth n l default =
  case l of
    [] -> default

    h :: t ->
      if n == 0 then h
      else dflNth (n-1) t default

{- drop nth item from list, or do nothing if list is too short -}
dflDropNth : Int -> List a -> List a
dflDropNth n l =
  case l of
    [] -> []

    h :: t ->
      if n == 0 then t
      else h :: dflDropNth (n-1) t

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

    _ -> (state, Cmd.none)

subscriptions : GameState -> Sub Msg
subscriptions model =
  Sub.none
