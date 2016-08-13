module DomingoView exposing(..)

import DomingoLib exposing(..)
import DomingoModel exposing(..)
import DomingoCards exposing(..)
import DomingoCardPrimitives exposing(..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing(..)
import Dict
import Set
import String


{- button generators -}
{- for buttons on hand cards -}
buttonGenHand pos =
  [tr [] [], button [ onClick (SubmitMove (PlayCard pos)) ] [ text ({-(toString pos) ++ -}" play") ]]

{- for buttons on shop cards (TODO) -}

{- output a card as HTML -}
{- takes card ID, its position in the list, and additional stuff to put at the bottom -}
{- also takes ID of the current player -}
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
{- TODO add player ID to the message -}
buttonGenShop cId =
  [tr [] [], button [ onClick (SubmitMove (BuyCard cId))]
                    [ text "Buy" ]]

{- display cards in shop -}
-- eventually this should take an input saying if it's your turn
displayShop shop ourTurn =
    let footer' cId = if ourTurn then buttonGenShop cId else []
        footer cId = footer' cId ++ [ text (" - Remaining: " ++ toString (dflGet cId shop 0))]
    in
  table []
        (List.foldl (\cId acc -> acc ++
                         [td [] [displayCard cId (footer cId)], td [] []])
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

-- display list of characters, and which are local        
displayAllPlayers gameState localPlayers =
    div [] <|
        [ text "Players playing:", br [] []] ++
            List.concatMap (\p -> if Set.member p localPlayers
                                  then [text (p ++ " (local)") , br [] []]
                                  else [text p, br [] []]) gameState.playerOrder

-- show the current player's amount of resources                
displayResources gameState currPlayer ourTurn =
    div []
        [ text "Current Player's resources:", br [] []
        , text ("Actions: " ++ toString gameState.actions), br [] []
        , text ("Coin: " ++ toString gameState.coin), br [] []
        , text ("Buys: " ++ toString gameState.buys), br [] []
        , br [] []]
        {-
        -- TODO prune/reorganize this information display
        , text ("Current Phase: " ++ toString gameState.phase)
        , br [] []
        , text ("Your deck has " ++ toString (List.length currPlayer.deck) ++ " cards")
        , br [] []
        , text (toString (List.length (gameState.plays)) ++ " cards have been played this turn; "
                    ++ (toString (List.length (gameState.purchases)) ++ " cards have been bought this turn"))
        ]
-}
    
    
-- display the state of the game
-- second argument is a list of local players, to determine if we
-- should show the buttons
displayGameState gameState localPlayers =
    let
        currPid = dflHead gameState.playerOrder dummyId
        currPlayer = dflGet currPid gameState.players dummy
        ourTurn = Set.member currPid localPlayers
        output =
            if gameState.phase == EndGamePhase
              then
                  displayWinners gameState
              else
                  -- show players connected
                  -- otherwise, show the game
                  div [] <|
                      [displayAllPlayers gameState localPlayers
                      , text ("It is currently " ++ toString currPid ++ "'s turn.")
                      , br [] []] ++
                      [text ("Current Phase: " ++ toString gameState.phase), br [] []] ++
                      [if ourTurn then displayHand currPlayer.hand
                       else text "not your turn."
                      , br [] []] ++
                      [displayResources gameState currPlayer ourTurn
                      , br [] []] ++
                      [displayShop gameState.shop ourTurn
                      , br [] []] ++
                      (if ourTurn then
                           [button [onClick (SubmitMove EndPhase)] [text "end phase/turn"]]
                       else []) ++
                      [br [] []]
                          
                      -- TODO: include more diagnostic/informative info about what happened

{-
 -}
    in output

-- TODO move this to DomingoLib
strOfMaybe : Maybe String -> String
strOfMaybe ms = case ms of
                    Nothing -> ""
                    Just s -> s

strOfMaybeInt : Maybe Int -> String
strOfMaybeInt mi = case mi of
                       Nothing -> ""
                       Just i -> toString i

-- add Html.Attributes.value

-- next, we need to be able to display connected players.
displayPlayerIdInputs cpgs =
    div [] <|
        (List.concatMap (\pId ->
                             [ text pId
                             , button [onClick (RemovePlayer pId)] [text "-"]
                             , br [] []
                             ]
                        ) cpgs.pidsInput) ++
          [ br [] [], br [] []
          , input [onInput UpdateNewPlayer, placeholder "Name of new player"
                  , value (strOfMaybe cpgs.newPidInput)] []
          , button [onClick AddNewPlayer] [text "+"]
          ]

displayPlayersConnected connected =
    div [] <| List.concatMap (\pId ->
                                  [ text pId
                                  , br [] []
                                  ]
                             ) connected

-- display button to start game if you are master
-- display game config options if you are master
displayMasterOptions clcState =
    div []
        [ button [onClick StartGame] [text "start game"], br [] []
        , input [onInput UpdateMasterSeed, placeholder "Optional seed; randomly generated if empty"
                , value (strOfMaybeInt clcState.rngInput)] []
        ]

-- display the main div
displayMainDiv clientState =
    case clientState of
        ClientPreGame cpgs ->
            div []
                [ h1 [] [text "Domingo"]
                , text "Who's playing? (If no players, you are a specator)", br [] []
                , displayPlayerIdInputs cpgs, br [] [], br [] []
                , text "Are you hosting? (Exactly 1 computer per game must.) ",
                    -- TODO i am unsure if "checkbox" is a valid type in elm's html system
                    input [type' "checkbox"
                          , checked cpgs.isMasterInput
                          , onCheck UpdateIsMaster] [], br [] []
                , input [onInput UpdateGameId, placeholder "Game ID"
                        , value (strOfMaybe cpgs.gidInput)] []
                , br [] []
                , button [onClick StartLobby]  [text "start game lobby"]
                ]

        ClientLobby cglState ->
            let pidText =
                    case cglState.playerIds of
                        [] -> "a spectator"
                        p1 :: ps -> "player " ++ p1
            in
            div [] <|
                [ h2 [] [text <|
                             "Waiting for friends. You are " ++ pidText ++
                             " and this is game " ++ cglState.gameId ++ " and you are" ++
                             (case cglState.masterConfigState of
                                  Just _ -> ""
                                  Nothing -> " not") ++ " the master."]
                , br [] [], br [] []
                -- TODO make sure "master has connected?" doesn't get out of sync with the players list
                , text ("Master has" ++ (if cglState.masterConnected then "" else " not") ++ " connected."), br [] []
                , text "Players connected: ", br [] []
                , displayPlayersConnected cglState.playerIds
                , br [] [], br [] []
                ] ++
                (case cglState.masterConfigState of
                     Just clc -> [displayMasterOptions clc]
                     Nothing -> [])

        -- add a "whose turn"
        ClientPlayMaster cpState ->
            displayGameState cpState.gameState cpState.localPlayerIds
  
        ClientPlaySub cpState ->
            -- TODO: if it's the turn of someone who's local,
            -- show the buttons!
            displayGameState cpState.gameState cpState.localPlayerIds

{- Displays some options that affect the entire
   client and are displayed regardless of game state -}
displayClientOptions clientState =
    let message =
            case
            (case clientState of
                 ClientPreGame cpg -> cpg.message
                 
                 ClientPlayMaster cp -> cp.message

                 ClientLobby cl -> cl.message
                                                           
                 ClientPlaySub cs -> cs.message
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
        -- TODO: eventually add a server maintenance button here? or somewhere
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
