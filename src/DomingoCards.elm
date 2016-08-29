module DomingoCards exposing(..)

import Dict
import Random
import DomingoLib exposing (..)
import DomingoModel exposing (..)
import DomingoCardPrimitives exposing(..)

{- Card definitions for the Domingo game. -}

{- TODO this does not belong here -}
playerStateIdFn : PlayerState -> PlayerState
playerStateIdFn ps = ps

gameStateIdFn : GameState -> GameState
gameStateIdFn gs = gs

{- we can build other cards from urCard, defined in DomingoCardPrimitives -}
{- better to use the following primitives though -}

{- primitives for building cards -}
makeCoin : CardId -> Int -> Int -> Card
makeCoin cId value cost =
    { urCard | idn = cId
    , spentValue = value
    , cost = cost
    , kind = "Money"
    , afterPlayEffect = putInPlays cId
    }

-- NB we can't play victory cards so no need for after play effect
-- TODO worry about this later, because no after play for victory
makeVictory : CardId -> Int -> Int -> Card
makeVictory cId victory cost =
    {urCard | idn = cId
    , kind = "Victory"
    , victory = victory
    , cost = cost }

-- NB this action has no PlayedEffect but _does_ have a default afterPlay
makeAction : CardId -> Int -> Card
makeAction cId cost =
    {urCard | idn = cId
    , cost = cost
    , kind = "Action"
    , afterPlayEffect = putInPlays cId}
           
{- TODO, perhaps have a hierarchy of these for each type of card -}

-- treasure
copperId = 0

copperCard =
    let c = makeCoin copperId 1 0 in
    {c | name = "Copper"
    , text = Just "Worth 1 Coin"}

silverId = 10

silverCard : Card
silverCard =
    let c = makeCoin silverId 2 3 in
    {c | name = "Silver"
    , text = Just "Worth 2 Coin" }
                  
goldId = 20

goldCard : Card
goldCard =
    let c = makeCoin goldId 3 6 in
    {c | name = "Gold"
    , text = Just "Worth 3 Coin"}

-- victory cards
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

duchyId = 1010
          
duchyCard : Card
duchyCard =
    { urCard | idn = duchyId
    , name = "Duchy"
    , text = Just "Worth 3 Victory"
    , kind = "Victory"
    , cost = 5
    , victory = 3 }

provinceId = 1020
             
provinceCard =
    { urCard | idn = provinceId
    , name = "Province"
    , text = Just "Worth 6 Victory"
    , kind = "Victory"
    , cost = 8
    , victory = 6 }

curseId = 1005

curseCard =
    { urCard | idn = curseId
    , name = "Curse"
    , text = Just "Worth -1 Victory"
    , kind = "Curse"
    , cost = 0
    , victory = -1 }

-- action cards
woodcutterId = 2000

woodcutterCard =
    let c = makeAction woodcutterId 3 in
    {c | name = "Woodcutter"
    , text = Just "+ 2 coin, +1 buy"
    , playedEffect = Just (\ost ->
                               case ost of
                                   GameState st ->
                                       GameState {st | buys = st.buys + 1
                                                 , coin = st.coin + 2})
    }

villageId = 2001

villageCard =
    let c = makeAction villageId 3 in
    {c | name = "Village"
    , text = Just "+1 card, +2 actions"
    , playedEffect = Just (\ost ->
                               case ost of
                                   GameState st ->
                                       GameState {st | actions = st.actions + 2} |> dealCurrentPlayerCards 1)
    }

smithyId = 2002
    
smithyCard =
    let c = makeAction smithyId 4 in
    { c | name = "Smithy"
    , text = Just "+3 Cards"
    , playedEffect = Just (dealCurrentPlayerCards 3) }


marketId = 2003

marketCard =
    let c = makeAction marketId 5 in
    { c | name = "Market"
    , text = Just "+1 coin, +1 card, +1 action, +1 buy"
    , playedEffect = Just (\ost ->
                               case ost of
                                   GameState st ->
                                       GameState {st | actions = st.actions + 1
                                                 , buys = st.buys + 1
                                                 , coin = st.coin + 1
                                                 } |> dealCurrentPlayerCards 1) }

festivalId = 2004

festivalCard =
    let c = makeAction festivalId 5 in
    { c | name = "Festival"
    , text = Just "+2 actions, +1 buy, +2 coins"
    , playedEffect = Just (\ost ->
                               case ost of
                                   GameState st ->
                                       GameState {st | actions = st.actions + 2
                                                 , buys = st.buys + 1
                                                 , coin = st.coin + 2 }) }

witchId = 2005

-- need to make an "iterator" function that threads state through
witchCard =
    let c = makeAction witchId 5 in
    { c | name = "Witch"
    , text = Just "+2 cards. Each other player gains a Curse"
    , playedEffect =
        Just (\ost ->
                  case ost of
                      GameState st ->
                          List.foldl (\pId -> gainPlayerCard pId curseId) ost
                              (dflTail st.playerOrder) -- NB we do tail so we don't give selves a curse
             )
    }

throneRoomId = 2006

throneRoomCard =
    { urCard | idn = throneRoomId
    , name = "Throne Room"
    , text = Just "Choose an action card from your hand. Play it twice."
    , kind = "Action"
    , cost = 4
    , playedEffect = Nothing} -- TODO fix, need a way to pick a card

laboratoryId = 2007

laboratoryCard =
    { urCard | idn = laboratoryId
    , name = "Laboratory"
    , text = Just "+2 cards, +1 action"
    , kind = "Action"
    , cost = 5
    , playedEffect = Just (\ost ->
                               case ost of
                                   GameState st ->
                                       GameState {st | actions = st.actions + 1} |> dealCurrentPlayerCards 2) }

councilRoomId = 2008

councilRoomCard =
    { urCard | idn = councilRoomId
    , name = "Council Room"
    , text = Just "+4 cards. Each other player gets +1 card."
    , kind = "Action"
    , cost = 5
    , playedEffect = Just (\ost -> dealCurrentPlayerCards 4 ost |>
                               \ost ->
                               case ost of
                                   GameState st ->
                                       List.foldl (\pId -> dealPlayerCard pId) ost (dflTail st.playerOrder)
                          )
    }

workshopId = 2009

workshopCard =
    { urCard | idn = workshopId
    , name = "Workshop"
    , text = Just "Gain a card costing up to 4 coins"
    , kind = "Action"
    , cost = 3
    , playedEffect = Nothing } -- TODO fix, need a gain primitive, choice primitive

-- need 2 more.

-- moneylender
-- gardens

{- global dictionary used to look up cards by ID -}

allCards : Dict.Dict CardId Card
allCards = Dict.fromList
            [ (urId, urCard)
            , (copperId, copperCard)
            , (estateId, estateCard)
            , (woodcutterId, woodcutterCard)
            , (villageId, villageCard)
            ]
