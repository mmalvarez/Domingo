{- separate file for actions that cards can use -}

module DomingoActions exposing(..)

import DomingoModel exposing(..)
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


{- dummy card used for lookups that the compiler thinks could fail -}
urId = -1

urCard : Card
urCard =
  { idn = urId, name = "Ur-Card", img = Nothing, text = Nothing
  , kind = "", victory = 0, spentValue = 0, cost = Random.maxInt
  , playedEffect = Nothing }

