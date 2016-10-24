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
playButtonGenHand cid =
  [tr [] [], button [ onClick (SubmitMove (PlayCard cid)) ] [ text "play" ]]

-- for "pick a card" from hand prompts
chooseButtonGenHand isOptional cid =
    let move = 
            if isOptional then
                POMaybeInt (Just cid)
            else POInt cid
    in
    [ tr [] [], button [ onClick <| SubmitMove <| PromptResponse move ] [text "select"]]
    

{- for buttons on shop cards -}
buyButtonGenShop cId =
  [tr [] [], button [ onClick (SubmitMove (BuyCard cId))]
                    [ text "Buy" ]]

chooseButtonGenShop isOptional cid =
    let move =
            if isOptional then
                POMaybeInt (Just cid)
            else POInt cid
    in
        [tr [] [], button [ onClick <| SubmitMove <| PromptResponse move ] [text "select"]]


            

{- output a card as HTML -}
{- takes card ID, its position in the list, and additional stuff to put at the bottom -}
{- also takes ID of the current player -}
displayCard cId footer =
  let c = unwrapCard <| dflGet cId allCards urCard in
  table [Html.Attributes.style [("outlineColor", "black"), ("outlineStyle", "solid")]]
        ([ tr [] [ td [] [strong [] [h3 [] [text c.name]]] ]
         , tr [] [ td [] [strong [] [text c.kind]]]
--         , tr [] [ td [] [text ("Victory: " ++ toString c.victory) ] ]
         , tr [] [ td [] [text ("Value: " ++ toString c.spentValue)] ]
         , tr [] [ td [] [text ("Cost: " ++ toString c.cost) ] ]
         , tr [] [ td [] [text (case c.text of
                                    Just s -> s
                                    _ -> "")]]] ++
           footer) -- TODO should footer really be in the table?

{- display your hand -}
{- outer div, then inner divs for each card. each card gets a name, description, and a play button -}
displayHand cards cardFooter =
  table [] <| List.foldl (\cId list -> list ++ [displayCard cId (cardFooter cId), td [] []]) [] cards


{- display cards in shop -}
-- TODO make this take a footer, just like display hand
-- the footer will be empty if user can't buy
-- may also reflect current prompt
displayShop shop footer' =
    let footer cId = footer' cId ++ [ text (" - Remaining: " ++ toString (dflGet cId shop 0))]
    in
  table []
        (List.foldl (\cId acc -> acc ++
                         [td [] [displayCard cId (footer cId)], td [] []])
         [] (Dict.keys shop))

displayWinnersTable gst =
  let
    scorers = Dict.foldl
                (\k v acc -> ((v.victory + scoreCards (v.deck ++ v.hand ++ v.discard) v allCards), k) :: acc)
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

-- display a (Maybe GamePrompt) as HTML
displayMaybePrompt mp =
    div [] <|
        case mp of
            Just gp ->
                -- TODO add more description?
                [text gp.desc, br [] []] ++
                    case gp.spec of
                        AcceptDecline ->
                            [ button [(onClick (SubmitMove (PromptResponse (POBool True))))]
                                  [text "Accept"], br [] []
                            , button [(onClick (SubmitMove (PromptResponse (POBool False))))]
                                [text "Decline"]]

                        ChooseCard decl cs ->
                            if decl then
                                let footer cId = [ tr [] []
                                                 , button [onClick (SubmitMove (PromptResponse
                                                                                    (POMaybeInt (Just cId))))]
                                                     [text "Choose"]]
                                in
                                    List.foldl (\cId l -> l ++ [displayCard cId (footer cId)]) [] cs ++
                                        [br [] []
                                        , button [onClick (SubmitMove (PromptResponse (POMaybeInt Nothing)))]
                                            [text "Decline"]]
                                             
                                        
                            else
                                let footer cId = [ tr [] []
                                                 , button [onClick (SubmitMove (PromptResponse
                                                                                    (POInt cId)))]
                                                     [text "Choose"]]
                                in
                                    List.foldl (\cId list -> list ++ [displayCard cId (footer cId)]) [] cs
                                                
                                

                        -- TODO: have a way to make displayHand do something
                        ChooseHandCard decl cs ->
                            if decl then
                                [button [onClick (SubmitMove (PromptResponse (POMaybeInt Nothing)))] [text "Decline (Choosing from Hand)"]]
                            else []

                        ChooseShopCard decl cs ->
                            if decl then
                                [button [onClick (SubmitMove (PromptResponse (POMaybeInt Nothing)))] [text "Decline (Choosing from Shop)"]]
                            else []
                                
                        -- this should never happen (corresponds to displaying an "unknown" prompt)
                        _ -> []
                        
            Nothing -> []
    
-- display the state of the game
-- second argument is a list of local players, to determine if we
-- should show the buttons
{- this needs to be updated to check prompt.
   do the following:
   if no prompt, do what we currently do
   if there is a "ChooseShopCard" prompt for the current player,
     then we change the shop footer
   if there is a "ChooseHandCard" prompt for the current player,
     then we change the hand footer
-}
displayGameState gameState' localPlayers =
    let
        gameState = unwrapGameState gameState'
        currPid = dflHead gameState.playerOrder dummyId
        currPlayer = dflGet currPid gameState.players dummy
        ourTurn = Set.member currPid localPlayers
        output =
            if gameState.phase == EndGamePhase
              then
                  displayWinners gameState
              else
                  -- check to see if we have a prompt
                  -- we need some kind of ourPrompt, to see if it is our prompt
                  -- if there is no prompt do what we do
                  -- otherwise we need to say something like "it is X's prompt"
                  -- and then display the prompt which may require changes to
                  -- card footer of shop or hand
                  
                  -- show players connected
                  -- otherwise, show the game

                  -- TODO rewrite this more maintainably
                  let shopButtonGen =
                          case gameState.prompt of
                              Just p ->
                                  case p.spec of
                                      ChooseShopCard b cIds ->
                                          \cId ->
                                              if List.member cId cIds && ourTurn
                                              then chooseButtonGenShop b cId
                                              else []
                                          
                                      _ -> if ourTurn then buyButtonGenShop else \_ -> []

                              _ ->  if ourTurn then buyButtonGenShop else \_ -> []
                      handButtonGen =
                          case gameState.prompt of
                              Just p ->
                                  case p.spec of
                                      ChooseHandCard b cIds ->
                                          \cId ->
                                               if List.member cId cIds
                                               then chooseButtonGenHand b cId
                                               else []

                                      _ -> if ourTurn then playButtonGenHand else \_ -> []
                              _ -> if ourTurn then playButtonGenHand else \_ -> []
                  in
                  div [] <|
                      [displayAllPlayers gameState localPlayers
                      , text ("It is currently " ++ toString currPid ++ "'s turn.")
                      , br [] []] ++
                      [text ("Current Phase: " ++ toString gameState.phase), br [] []] ++
                      [if ourTurn then displayHand currPlayer.hand handButtonGen
                       else text "not your turn."
                      , br [] []] ++
                      [displayResources gameState currPlayer ourTurn
                      , br [] []] ++
                      [displayShop gameState.shop shopButtonGen
                      , br [] []] ++
                      (if ourTurn then
                           [button [onClick (SubmitMove EndPhase)] [text "end phase/turn"]]
                       else []) ++ [br [] []] ++
                      [displayMaybePrompt gameState.prompt, br [] []]
                      -- TODO: include more diagnostic/informative info about what happened; e.g. log

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

-- display button to start game, config options, if you are master
displayMasterOptions clcState =
    div []
        [ button [onClick StartGame] [text "start game"], br [] []
        , input [onInput UpdateMasterSeed, placeholder "Optional seed; randomly generated if empty"
                , value (strOfMaybeInt clcState.rngInput)] []
        ]

extractGameLog : ClientState -> List MoveDesc
extractGameLog cs =
    let log = case cs of
                    ClientPlayMaster cpState -> cpState.gameLog
                    ClientPlaySub cpState -> cpState.gameLog
                    _ -> []
    in List.concatMap (\le ->
                           case le of
                               LoggedMove md -> [md]
                               _ -> []) log

extractChatLog : ClientState -> List (PlayerId, String)
extractChatLog cs =
    let log = case cs of
                  ClientPlayMaster cpState -> cpState.gameLog
                  ClientPlaySub cpState -> cpState.gameLog
                  _ -> []
    in List.concatMap (\le ->
                           case le of
                               LoggedChatMessage p m -> [(p,m)]
                               _ -> []) log

-- display the log of chat messages
displayChatDiv clientState =
    let chats = extractChatLog clientState in
    let newChat =
            case clientState of
                ClientPlayMaster cp -> cp.chatBox
                ClientPlaySub cp -> cp.chatBox
                _ -> Nothing
    in
    div [] <|
        [h2 [] [text "Messages:"] ] ++
        List.concatMap (\(p, m) ->
            [text (p ++ ": " ++ m), br [] []]) chats ++
        [ button [onClick SendChat] [text "Chat"]
        , input [ onInput UpdateChatBox
                , placeholder "type a message"
                , value (strOfMaybe newChat)] []
        ]

displayMoveDesc md =
    case md.play of
        PlayCard cid ->
            text (md.playerId ++ " played " ++ (unwrapCard (dflGet cid allCards urCard)).name)
        BuyCard cid ->
            text (md.playerId ++ " bought " ++ (unwrapCard (dflGet cid allCards urCard)).name)
        EndPhase ->
            text (md.playerId ++ " ended phase")
        PromptResponse po ->
            let selectText =
                    case po of
                        POBool b -> "(bool) " ++ toString b
                        POInt i -> "(int)" ++ toString i
                        POMaybeInt (Just i) ->
                            "(maybe int) " ++ toString i
                        POMaybeInt Nothing -> "nothing"
                        POUnknown -> "unknown"
            in text (md.playerId ++ " selected " ++ selectText)

-- display the play log
displayGameLogDiv clientState =
    let moves = extractGameLog clientState in
    div [] <|
        [h2 [] [text "Game Log:"] ] ++
        List.concatMap (\m -> [displayMoveDesc m, br [] []]) moves

-- display the main div
displayMainDiv clientState =
    case clientState of
        ClientPreGame cpgs ->
            div []
                [ h1 [] [text "Domingo"]
                , text "Who's playing? (If no players, you are a specator)", br [] []
                , displayPlayerIdInputs cpgs, br [] [], br [] []
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
      chatLogDiv = displayChatDiv clientState
      gameLogDiv = displayGameLogDiv clientState
  in
  div []
    ([ mainDiv ] ++
      [ br [] [], br [] []] ++
      [ gameLogDiv, br [] [], br [] []] ++
      [ chatLogDiv, br [] [], br [] []] ++
      [ clientOptions ] ++
      [ br [] [], br [] []
      , text "Created By Ronald X Hackerino"])
