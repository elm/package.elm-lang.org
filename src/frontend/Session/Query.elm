module Session.Query exposing
  ( Query
  , check
  , success
  , failure
  , required
  , maybe
  , map, map2, map3
  , andThen
  )


import Session.Status as Status exposing (Status(..))



-- QUERY


type Query state msg err result
  = Query ( state -> ( state, Cmd msg, Status err result ) )


check : state -> Query state msg err result -> ( state, Cmd msg, Status err result )
check state (Query query) =
  query state



-- CREATE YOUR OWN


success : result -> Query state msg err result
success result =
  Query <| \state -> ( state, Cmd.none, Success result )


failure : err -> Query state msg err result
failure err =
  Query <| \state -> ( state, Cmd.none, Failure err [] )


required : ( state -> ( state, Cmd msg, Status result ) ) -> Query state msg result
required =
  Query



-- OPTIONAL QUERIES


maybe : Query s m e a -> Query s m e (Maybe a)
maybe (Query query) =
  Query <|
    \state0 ->
      let
        (state1, cmds, status) =
          query state0
      in
      ( state1, cmds, makeOptional status )


makeOptional : Status x a -> Status x (Maybe a)
makeOptional status =
  case status of
    Failure e es ->
      Failure e es

    Loading ->
      Success Nothing

    Success a ->
      Success (Just a)



-- MAPPING


map : (a -> b) -> Query s m e a -> Query s m e b
map func (Query query) =
  Query <|
    \state0 ->
      let
        (state1, cmds, status) =
          query state0
      in
      ( state1, cmds, Status.map func status )


map2 : (a -> b -> result) -> Query s m e a -> Query s m e b -> Query s m e result
map2 func (Query aQuery) (Query bQuery) =
  Query <|
    \state0 ->
      let
        (state1, cmds1, aStatus) = aQuery state0
        (state2, cmds2, bStatus) = bQuery state1
      in
      ( state2, Cmd.batch [ cmds1, cmds2 ], Status.map2 func aStatus bStatus )


map3 : (a -> b -> c -> result) -> Query s m e a -> Query s m e b -> Query s m e c -> Query s m e result
map3 func (Query aQuery) (Query bQuery) (Query cQuery) =
  Query <|
    \state0 ->
      let
        (state1, cmds1, aStatus) = aQuery state0
        (state2, cmds2, bStatus) = bQuery state1
        (state3, cmds3, cStatus) = cQuery state2
      in
      ( state3, Cmd.batch [ cmds1, cmds2, cmds3 ], Status.map3 func aStatus bStatus cStatus )



-- AND THEN


andThen : (a -> Query s m e b) -> Query s m e a -> Query s m e b
andThen callback (Query aQuery) =
  Query <|
    \state0 ->
      let
        (state1, cmds1, aStatus) =
          aQuery state0
      in
      case aStatus of
        Failure e es ->
          ( state1, cmds1, Failure e es )

        Loading ->
          ( state1, cmds1, Loading )

        Success a ->
          let
            (Query bQuery) =
              callback a

            (state2, cmds2, bStatus) =
              bQuery state1
          in
          ( state2, Cmd.batch [ cmds1, cmds2 ], bStatus )
