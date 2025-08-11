module RichText.Model.State exposing (State, state, root, selection, withRoot, withSelection)

{-| A `State` consists of a root block and a selection. `State` allows you to keep
track of and manipulate the contents of the editor.

@docs State, state, root, selection, withRoot, withSelection

-}

import RichText.Annotation exposing (addAtPath, clear)
import RichText.Internal.Constants exposing (focusingAnnotation)
import RichText.Model.Node exposing (Block)
import RichText.Model.Selection exposing (Selection, focusNode)
import RichText.Node exposing (findTextBlockNodeAncestor)


{-| A `State` consists of a root block and a selection. `State` allows you to keep
track of and manipulate the contents of the editor.
-}
type State
    = State Contents


type alias Contents =
    { root : Block
    , selection : Maybe Selection
    , lastSelection : Maybe Selection
    }


{-| Creates a `State`. The arguments are as follows:

  - `root` is a block node that represents the root of the editor.

  - `selection` is a `Maybe Selection` that is the selected part of the editor

```
root : Block
root =
    block
        (Element.element doc [])
        (blockChildren <|
            Array.fromList
                [ block
                    (Element.element paragraph [])
                    (inlineChildren <| Array.fromList [ plainText "" ])
                ]
        )

state root Nothing
--> an empty editor state with no selection
```

-}
state : Block -> Maybe Selection -> Maybe Selection -> State
state root_ sel_ lasel =
    State { root = root_, selection = sel_, lastSelection = lasel }


{-| the selection from the state
-}
selection : State -> Maybe Selection
selection st =
    case st of
        State s ->
            s.selection


{-| the root node from the state
-}
root : State -> Block
root st =
    case st of
        State s ->
            s.root


{-| a state with the given selection
this will store the old selection into lastSelection
-}
withSelection : Maybe Selection -> State -> State
withSelection sel st =
    case st of
        State s ->
            State
                { s
                    | selection = sel
                    , lastSelection = s.selection
                    , root =
                        s.root
                            |> clear focusingAnnotation
                            |> addFocusingAnnotation sel
                }


{-| a state with the given root
-}
withRoot : Block -> State -> State
withRoot node st =
    case st of
        State s ->
            State { s | root = node }



-- Helpers


addFocusingAnnotation : Maybe Selection -> Block -> Block
addFocusingAnnotation sel_ rootBlock =
    case sel_ of
        Just sel ->
            case findTextBlockNodeAncestor (focusNode sel) rootBlock of
                Just ( focusingPath, _ ) ->
                    addAtPath focusingAnnotation focusingPath rootBlock
                        |> Result.withDefault rootBlock

                Nothing ->
                    rootBlock

        Nothing ->
            rootBlock
