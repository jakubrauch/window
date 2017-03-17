effect module Window where { subscription = MySub } exposing
  ( Size, Offset
  , size, width, height, offset, x, y
  , resizes, scrolls
  )

{-| Your application lives in some sort of window. This library helps you
figure out how big that window is.

# Window Size
@docs Size, size, width, height, resizes

# Window Offset
@docs Offset, offset, x, y, scrolls
-}

import Dom.LowLevel as Dom
import Json.Decode as Json
import Native.Window
import Process
import Task exposing (Task)


type Effect
  = Resized Size
  | Scrolled Offset

{-| The size of the window in pixels.
-}
type alias Size =
  { width : Int
  , height : Int
  }

{-| The scroll offset of the window in pixels.
-}
type alias Offset =
  { x : Int
  , y : Int
  }


{-| Get the current window size.
-}
size : Task Never Size
size =
  Native.Window.size


{-| Get the current window width.
-}
width : Task Never Int
width =
  Task.map .width size


{-| Get the current window height.
-}
height : Task Never Int
height =
  Task.map .height size


{-| Get the current window offset.
-}
offset : Task Never Offset
offset =
  Native.Window.offset


{-| Get the current window horizontal scroll offset.
-}
x : Task Never Int
x =
  Task.map .x offset


{-| Get the current window vertical scroll offset.
-}
y : Task Never Int
y =
  Task.map .y offset


{-| Subscribe to any changes in window size.
-}
resizes : (Size -> msg) -> Sub msg
resizes tagger =
  subscription (Resizes tagger)


{-| Subscribe to any window scrolls.
-}
scrolls : (Offset -> msg) -> Sub msg
scrolls tagger =
  subscription (Scrolls tagger)



-- SUBSCRIPTIONS


type MySub msg
  = Resizes (Size -> msg)
  | Scrolls (Offset -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func mySub =
  case mySub of
    Resizes tagger -> Resizes (tagger >> func)
    Scrolls tagger -> Scrolls (tagger >> func)



-- EFFECT MANAGER


type alias State msg =
    { subs : List (MySub msg)
    , resizePid : Maybe Process.Id
    , scrollPid : Maybe Process.Id
    }


init : Task Never (State msg)
init =
  Task.succeed { subs = [], resizePid = Nothing, scrollPid = Nothing }


(&>) task1 task2 =
  Task.andThen (\_ -> task2) task1


onEffects : Platform.Router msg Effect -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router newSubs oldState =
  let
    resizeSubs = List.filter (\sub -> isResizes sub) newSubs
    scrollSubs = List.filter (\sub -> isScrolls sub) newSubs
  in
    Task.map2
      (\rPid sPid -> { oldState | resizePid = rPid, scrollPid = sPid, subs = newSubs })
      (onResizePid router resizeSubs oldState.resizePid)
      (onScrollPid router scrollSubs oldState.scrollPid)


onResizePid : Platform.Router msg Effect -> List (MySub msg) -> Maybe Process.Id -> Task x (Maybe Process.Id)
onResizePid router subs maybePid =
  case (maybePid, subs) of
    (Just pid, []) -> kill pid
    (Nothing, _ :: _) -> spawnListener router "resize" (Task.map Resized size)
    _ -> Task.succeed maybePid


onScrollPid : Platform.Router msg Effect -> List (MySub msg) -> Maybe Process.Id -> Task x (Maybe Process.Id)
onScrollPid router subs maybePid =
  case (maybePid, subs) of
    (Just pid, []) -> kill pid
    (Nothing, _ :: _) -> spawnListener router "scroll" (Task.map Scrolled offset)
    _ -> Task.succeed maybePid


kill : Process.Id -> Task x (Maybe a)
kill pid = Process.kill pid &> Task.succeed Nothing


spawnListener : Platform.Router msg Effect -> String -> Task Never Effect -> Task x (Maybe Process.Id)
spawnListener router event fx =
 Process.spawn (Dom.onWindow event (Json.succeed ()) (\_ -> Task.andThen (Platform.sendToSelf router) fx))
        |> Task.andThen (\pid -> Task.succeed (Just pid))


isResizes : MySub msg -> Bool
isResizes sub =
  case sub of
    Resizes _ -> True
    _ -> False


isScrolls : MySub msg -> Bool
isScrolls sub =
  case sub of
    Scrolls _ -> True
    _ -> False


onSelfMsg : Platform.Router msg Effect -> Effect -> State msg -> Task Never (State msg)
onSelfMsg router fx state =
  case (fx, state.subs) of
    (_, []) -> Task.succeed state

    (Scrolled offset, subs) ->
          let
            send mySub =
              case mySub of
                Scrolls tagger -> Platform.sendToApp router (tagger offset)
                _ -> Task.succeed ()
          in
            Task.sequence (List.map send subs)
              &> Task.succeed state

    (Resized dimensions, subs) ->
          let
            send mySub =
              case mySub of
                Resizes tagger -> Platform.sendToApp router (tagger dimensions)
                _ -> Task.succeed ()
          in
            Task.sequence (List.map send subs)
              &> Task.succeed state

