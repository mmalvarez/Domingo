{- messages/data model for Domingo client -}
module DomingoModel exposing(..)

import Dict
import Task

{- TODO make these types have constructors -}
type alias PlayerId = Int
type alias CardId = Int
type alias GameId = String

{- state of each player in a game -}
type alias PlayerState =
  { deck : List CardId
  , discard : List CardId
  , hand : List CardId
  , victory : Int {- Extra VP earned from e.g. actions -}
  , valid : Bool
  }

{- whose turn is it? is the game over? -}
{- TODO: use continuations to have "response" phases -}
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
  , playedEffect : Maybe (GameState -> GameState)
  {- to do: some way of specifying reactions when it's in your hand -}
  {- to do: some way of specifying reactions when it's on the field -}
}

{- State of a Domingo game in progress.
   This (eventually, deltas to it) are what get serialized over
   the wire and sent to clients
-}
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
  , phase : GamePhase {- NB this used to be conflated with client state -}
  , rng : Int
  {- used at end of game to display how well people did
     in number of victory points -}
  , winners : List (PlayerId, Int)
  , message : String {- used for debugging -}
  , gameId : GameId
}

{- For chaining client actions -}
type alias ClientTask = Task.Task String ClientState

{- Definitions for client-side state -}
type alias ClientPreGameState =
  { gidInput : Maybe GameId
  , rngInput : Maybe Int
  , message : Maybe String }

type alias ClientPlayState =
  { gameId : GameId
  , gameState : GameState {- game state should have .gameId == gId -}
  , message : Maybe String
  }

type alias ClientSpectateState =
  { gameId : GameId
  , gameState : Maybe GameState {- nothing if the server hasn't sent us a message -}
  , message : Maybe String }

type ClientState =
       ClientPreGame ClientPreGameState
     | ClientPlay ClientPlayState
     | ClientSpectate ClientSpectateState

{- Messages used by main app -}
type Msg = GotServerMsg String
          {- for debugging -}
          | ShowState
          {- useful ones -}
          {- need ones for manually/automatically setting RNG -}
          | UpdateGameId
          | StartGame
          | SpectateGame
          | QuitGame
          | PlayCard Int
          | EndPhase
          | BuyCard CardId
          | EndBuy
          {- message sent when new random seed is generated -}
          {- might want to deprecate this one -}
          | InitRandom Int
          {- if a task fails - should be a no op, possibly log error -}
          | TaskFail String
          {- general call to update client's state -}
          | UpdateClientState ClientState
