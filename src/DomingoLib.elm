{- Contains miscellaneous library functions.
   Keep external dependencies to a minimum; maybe it could have outside use.
 -}

module DomingoLib exposing(..)

import List
import Dict
import Json.Encode as Encode
import Json.Decode as Decode exposing(..)

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

{- variant of Dict.get that takes a default var -}
dflGet : comparable -> Dict.Dict comparable b -> b -> b
dflGet p d default = case Dict.get p d of
                      Just r -> r
                      Nothing -> default

{- get nth item in list (takes a default value) -}
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

{- our implementation of Json.extras.apply
   for encoding objects of arbitrary arity
   usually the Decoder a will be ("var" := val) -}
(|:) : Decoder (a -> b) -> Decoder a -> Decoder b
(|:) df da =
    -- want to "take out" a -> b, and map it into
    df `andThen` \g -> map g da
