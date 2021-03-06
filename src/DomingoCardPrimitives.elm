{- Implementation of primitives such as shuffling a deck,
   picking cards, etc.
   TODO give this a better name.
 -}
module DomingoCardPrimitives exposing(..)

import DomingoModel exposing(..)
import DomingoLib exposing(..)
import Dict
import Random
import Task exposing(succeed)

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


{- run tasks on a client state; almost certainly defunct -}
{- doClientTask : ClientTask -> Cmd Msg
doClientTask = Task.perform (\str -> TaskFail str)
                            (\state -> UpdateClientState state)
-}

{- update'. this function updates a value in a Dict. it is a noop if key is not there -}
update' : comparable -> (v -> v) -> Dict.Dict comparable v -> Dict.Dict comparable v
update' k f d =
  case (Dict.get k d) of
    Just v -> Dict.insert k (f v) d
    Nothing -> d

{- shuffle player's discard pile into their deck -}
{- this is a no-op unless player has an empty deck and nonempty discard pile -}
{- NOTE affects the RNG -}
shufflePlayerDeck : PlayerId -> GameState -> GameState
shufflePlayerDeck pId ost =
    case ost of
        GameState st ->
            let p = dflGet pId st.players dummy in
            if (not <| List.isEmpty p.deck) || (List.isEmpty p.deck && List.isEmpty p.discard)
            then
                GameState st
            else
                let (newDeck, newSeed) = shuffle' p.discard <| Random.initialSeed st.rng in
                GameState
                {st | rng = intOfSeed newSeed
                , players = update' pId
                            (\p -> {p | deck = newDeck, discard = []})
                                st.players
                }
        
{- Deal the given player a card from the top of their deck -}
{- NOTE affects the RNG (if it triggers deck shuffling) -}
dealPlayerCard : PlayerId -> GameState -> GameState
dealPlayerCard pId ost =
    case ost of
        GameState st ->
            let p = dflGet pId st.players dummy in
            {- if there is a card to deal, deal it -}
            if (not <| List.isEmpty p.deck)
            then
                GameState {st | players = update' pId
                               (\p -> {p | deck = dflTail p.deck, hand = dflHead p.deck urId :: p.hand})
                               st.players
                          }
            else
                if (not <| List.isEmpty p.discard)
                {- shuffle and retry if the discard isn't empty
                (at this point we know deck is also empty)
                 -}
                then
                    shufflePlayerDeck pId ost |> dealPlayerCard pId
                        
                {- otherwise, no-op -}
                else ost

dealPlayerCards : PlayerId -> Int -> GameState -> GameState
dealPlayerCards p n st =
  if n == 0 then st
  else
      dealPlayerCard p st |> dealPlayerCards p (n-1)

{- deal a card to current player (at front of rotation) -}
dealCurrentPlayerCards : Int -> GameState -> GameState
dealCurrentPlayerCards n ost =
    case ost of
        GameState st ->
            let current = dflHead st.playerOrder dummyId in
            dealPlayerCards current n ost

rotatePlayers : GameState -> GameState
rotatePlayers ost =
    case ost of
        GameState st ->
            GameState {st | playerOrder = List.drop 1 st.playerOrder ++ List.take 1 st.playerOrder}

-- returns the card added (or none) and new shop state, after we gain a card
gainCardSub : CardId -> GameState -> (List CardId, Dict.Dict CardId Int)
gainCardSub cId ogs =
    case ogs of
        GameState gs ->
            case Dict.get cId gs.shop of
                Just n ->
                    if n > 0 then
                        ([cId], Dict.insert cId (n-1) gs.shop)
                    else ([], gs.shop)
                Nothing ->
                    ([], gs.shop)

-- the "default"
putInPlays : CardId -> GameState -> GameState
putInPlays cId ogs =
    case ogs of
        GameState gs ->
            GameState {gs | plays = gs.plays ++ [cId]}

-- for e.g. feast; cards we trash after playing
putInTrash : CardId -> GameState -> GameState
putInTrash cId ogs =
    case ogs of
        GameState gs ->
            GameState {gs | trash = gs.trash ++ [cId]}
                        
-- cause the current player to gain a card.
-- NB this card will go into the "purchases" pile
-- noop if the card is not in library or has run out
gainCurrentPlayerCard : CardId -> GameState -> GameState
gainCurrentPlayerCard cId ogs =
    case ogs of
        GameState gs ->
            let (cardToAdd, newShop) = gainCardSub cId ogs
            in
                GameState {gs | purchases = gs.purchases ++ cardToAdd
                          , shop = newShop}

-- cause a player to "gain" a card (by ID)
-- if that card is not in the library, or is out, it's a noop
gainPlayerCard : PlayerId -> CardId -> GameState -> GameState
gainPlayerCard pId cId ogs =
    case ogs of
        GameState gs ->
            case Dict.get pId gs.players of
                Just ps ->
                    let (cardToAdd, newShop) = gainCardSub cId ogs in
                    GameState {gs | shop = newShop
                              , players = Dict.insert pId
                                          {ps | discard = ps.discard ++ cardToAdd} gs.players}
                Nothing -> ogs

initialDeal : GameState -> GameState
initialDeal ost =
    case ost of
        GameState st ->
    {- check if player 1 has a hand -}
            let
                nextId = (dflHead st.playerOrder dummyId)
                nextPlayer = dflGet nextId st.players dummy
            in
                {- make sure we don't deal to dummy player, probably unnecessary -}
                if nextPlayer.valid then
                    {- at this point, we have dealt to everyone already -}
                    if not (List.isEmpty nextPlayer.hand) then
                        {- start the first action phase -}
                        GameState {st | phase = ActionPhase
                                  , actions = 1
                                  , coin = 0
                                  , buys = 1
                                  }
                        
                    else
                        {- if not, deal them a hand -}
                        shufflePlayerDeck nextId ost |>
                        dealPlayerCards nextId 5 |>
                        rotatePlayers |>
                        initialDeal
                else ost

{- sum how many victory points a list of cards is worth 
   should be passed the list of cards to check (allCards); this is to
   break the dependency on DomingoCards
   we pass a PlayerState because we may depend on player's deck
   (e.g. gardens)
-}
scoreCards : List CardId -> PlayerState -> Dict.Dict CardId Card -> Int
scoreCards l ps allCards =
  List.foldl (\cId i ->
              let c = unwrapCard <| dflGet cId allCards urCard in
              c.victory ps + i) 0 l

{- turn a list of players into a Dict with initial player states (as specified in DomingoConf) -}
{- TODO make this more tunable? -}
{- TODO move this out somewhere that makes sense -}
initPlayers : PlayerState -> List PlayerId -> Dict.Dict PlayerId PlayerState
initPlayers start =
    List.foldl (\i d -> Dict.insert i start d) Dict.empty   

{- dummy card used for lookups that the compiler thinks could fail -}
urId = -1

uc' =
    { idn = urId, name = "Ur-Card", img = Nothing, text = Nothing
    , kind = "", victory = (\_ -> 0), spentValue = 0, cost = Random.maxInt
    , playedEffect = Nothing, afterPlayEffect = (\x -> x) }
      
urCard : Card
urCard = Card uc'
