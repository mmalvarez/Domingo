{- messages/data model for Domingo client -}
module DomingoModel exposing(..)

import Dict

{- import the Domingo cards. -}
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
{- use continuations to have "response" phases -}
type GamePhase = PreGamePhase
               | DealPhase
               | ActionPhase
               | CoinPhase
               | BuyPhase
               | EndGamePhase {- game is over -}


type alias Card =
  { idn : CardId
  , name : String
  , img : Maybe String
  , text : Maybe String
  {- TODO maybe make this semantic. is it action, victory, etc -}
  , kind : String 
  , victory : Int {- how much victory is this worth at end of game? -}
  , spentValue : Int {- how much is it worth if played as money? -}
  , cost : Int {- how much does it cost to buy from the shop? -}
  {- what happens when you play it as an action -}
  , playedEffect : Maybe (GameState -> (GameState, Cmd Msg)) 
  {- to do: some way of specifying reactions when it's in your hand -}
  {- to do: some way of specifying reactions when it's on the field -}
}

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
  , message : String {- used for debugging -}
}

type Msg = IncrementCoin | DecrementCoin
          | GotServerMsg String
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
