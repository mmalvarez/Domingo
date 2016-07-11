module DomingoCards exposing(..)

import Dict
import Random
import DomingoModel exposing (..)
import DomingoActions exposing(..)

{- Card definitions for the Domingo game. -}

{- TODO this does not belong here -}
playerStateIdFn : PlayerState -> PlayerState
playerStateIdFn ps = ps

gameStateIdFn : GameState -> GameState
gameStateIdFn gs = gs

{- we build other cards from urCard, defined in DomingoActions -}

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
           , playedEffect = Just (\st -> {st | buys = st.buys + 1
                                             , coin = st.coin + 2})
  }

villageId = 2001

villageCard =
  { urCard | idn = villageId
           , name = "Village"
           , text = Just "1 card and 2 actions"
           , kind = "Action"
           , cost = 3
           , playedEffect = Just (\st -> {st | actions = st.actions + 2} |> dealCurrentPlayerCards 1)
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
