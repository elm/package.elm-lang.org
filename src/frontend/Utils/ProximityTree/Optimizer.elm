module Utils.ProximityTree.Optimizer
    ( layout
    )
    where


type alias Edge = Float


type alias Node a =
  { payload : a
  , minRightSpacing : Float
  }


-- a list of items and their relative distances to the following item
type alias ProximityGraph a =
  List (Node a, Maybe Edge)


layout : (a -> Float) -> List (Float, a) -> List (Float, a)
layout toDesiredSpacing list =
  list
    |> fromList toDesiredSpacing
    |> reflow 0.0
    |> shrinkIfNecessary softShrink
    |> shrinkIfNecessary hardShrink
    |> toList


fromList : (a -> Float) -> List (Float, a) -> ProximityGraph a
fromList toDesiredSpacing list =
  if List.isEmpty list then
    []
  else
    let
      (fractions, values) =
        List.unzip list

      nextFractions =
        (List.map Just fractions) ++ [Nothing]
          |> List.drop 1

      spacings =
        List.map toDesiredSpacing values

      nextSpacings =
        spacings ++ [0.0]
          |> List.drop 1
    in
      List.map5 makeEntry spacings nextSpacings values fractions nextFractions


makeEntry : Float -> Float -> a -> Float -> Maybe Float -> (Node a, Maybe Edge)
makeEntry spacing nextSpacing value fraction nextFraction =
  ( Node value (0.5 * (spacing + nextSpacing))
  , Maybe.map2 (-) nextFraction (Just fraction)
  )


reflow : Float -> ProximityGraph a -> ProximityGraph a
reflow debt graph =
  case graph of
    [] ->
      []

    ((_, Nothing) as entry) :: rest ->
      entry :: rest

    (node, Just right) :: rest ->
      let
        adjustedRight =
          right - debt

        want =
          max 0.0 (node.minRightSpacing - adjustedRight)

        newNode =
          ( node
          , Just (adjustedRight + want)
          )
      in
        newNode :: reflow want rest


amountToShrink : ProximityGraph a -> Maybe Float
amountToShrink graph =
  let
    distances =
      List.map (rightEdge >> Maybe.withDefault 0.0) graph

    overflow =
      List.sum distances - 1.0
  in
    if overflow > 0.0000001 then Just overflow else Nothing


shrinkIfNecessary : (Float -> ProximityGraph a -> ProximityGraph a) -> ProximityGraph a -> ProximityGraph a
shrinkIfNecessary shrinker graph =
  case amountToShrink graph of
    Nothing ->
      graph

    Just overflow ->
      shrinker overflow graph


-- soft because it collapses space without causing overlap
softShrink : Float -> ProximityGraph a -> ProximityGraph a
softShrink overflow graph =
  case graph of
    [] ->
      []

    ((_, Nothing) as entry) :: rest ->
      entry :: softShrink overflow rest

    (node, Just right) :: rest ->
      let
        excess =
          max 0.0 (right - node.minRightSpacing)

        want =
          min excess overflow

        remainingOverflow =
          overflow - want  

        newNode =
          ( node
          , Just (right - want)
          )
      in
        newNode :: softShrink remainingOverflow rest


-- hard because it may result in overlap (use this as a last resort)
hardShrink : Float -> ProximityGraph a -> ProximityGraph a
hardShrink overflow graph =
  let
    adjustment =
      overflow / (toFloat (List.length graph))

    adjuster (node, maybeEdge) =
      ( node
      , Maybe.map2 (-) maybeEdge (Just adjustment)
      )
  in
    List.map adjuster graph


toList : ProximityGraph a -> List (Float, a)
toList graph =
  let
    adder a b =
      b + (Maybe.withDefault 0.0 a)

    fractions =
      List.scanl
        adder
        0.0
        (List.map rightEdge graph)

    values =
      List.map (node >> .payload) graph
  in
    List.map2 (,) fractions values


-- UTILITY

node : (Node a, Maybe Edge) -> Node a
node (node, _) =
  node


rightEdge : (Node a, Maybe Edge) -> Maybe Edge
rightEdge (_, right) =
  right
