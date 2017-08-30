module Session.Status exposing
  ( Status(..)
  , map
  , map2
  , map3
  , mapError
  )



-- STATUS


type Status err result
  = Failure err (List err)
  | Loading
  | Success result



-- MAP ERROR


mapError : (x -> y) -> Status x a -> Status y a
mapError func status =
  case status of
    Failure err errors ->
      Failure (func err) (List.map func errors)

    Loading ->
      Loading

    Success a ->
      Success a



-- MAPPING


map : (a -> b) -> Status x a -> Status x b
map func status =
  case status of
    Failure err errors ->
      Failure err errors

    Loading ->
      Loading

    Success a ->
      Success (func a)


map2 : (a -> b -> result) -> Status x a -> Status x b -> Status x result
map2 func aStatus bStatus =
  case (aStatus, bStatus) of
    (Success a, Success b) ->
      Success (func a b)

    _ ->
      case toErrors aStatus ++ toErrors bStatus of
        [] ->
          Loading

        err :: errors ->
          Failure err errors


map3 : (a -> b -> c -> result) -> Status x a -> Status x b -> Status x c -> Status x result
map3 func aStatus bStatus cStatus =
  case (aStatus, bStatus, cStatus) of
    (Success a, Success b, Success c) ->
      Success (func a b c)

    _ ->
      case toErrors aStatus ++ toErrors bStatus ++ toErrors cStatus of
        [] ->
          Loading

        err :: errors ->
          Failure err errors


toErrors : Status x a -> List x
toErrors status =
  case status of
    Failure e es ->
      e :: es

    Loading ->
      []

    Success _ ->
      []
