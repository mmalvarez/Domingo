module DomingoView exposing(..)

import DomingoLib exposing(..)
import DomingoModel exposing(..)
import DomingoCards exposing(..)
import DomingoCardPrimitives exposing(..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(..)
import Dict


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
                (\k v acc -> ((v.victory + scoreCards (v.deck ++ v.hand ++ v.discard) allCards), k) :: acc)
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
