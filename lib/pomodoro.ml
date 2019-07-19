type timer_definition = {
  work: int;
  break: int;
  long_break: int;
}

let timer_defs = [|
  {work = 25 * 60; break = 5 * 60; long_break = 30 * 60};
  {work = 50 * 60; break = 10 * 60; long_break = 60 * 60};
  {work = 15 * 60; break = 5 * 60; long_break = 15 * 60};
|]

let string_of_timerdef x = Printf.sprintf "%d/%d/%d"
                             (x.work / 60) (x.break / 60) (x.long_break / 60)

let timestring_of_seconds x = Printf.sprintf "%dm" (((x - 1) / 60) + 1)

let tap f x = f x; x

type update_msg =
  | Play
  | Tick of int
  | Next

module Pomodoro = struct
  type state =
    | Idle
    | Setting of int
    | WaitWork of int
    | Work of int
    | WaitRest of int
    | Rest of int
    | Finished

  type model = {
    state: state;
    cycle: int;
    def: timer_definition;
  }

  let describe_state = function
    | Idle -> "Idle"
    | Setting _ -> "Setting durations"
    | WaitWork _ -> "Waiting to start work"
    | Work _ -> "Work began"
    | Rest _ -> "Rest began"
    | WaitRest _ -> "Waiting to start rest"
    | Finished -> "End"

  let update ?(notify_state_change=print_endline) m msg =
    let send_notification {state; _} = state
                                       |> describe_state
                                       |> notify_state_change in
    match msg, m.state with
    | Play, Idle      -> {m with state = Setting 0}
    | Play, Setting i -> let def = timer_defs.(i) in {def;
                                                      state = Work def.work;
                                                      cycle = 0}
    | Play, Work _
    | Play, Rest _
    | Play, Finished   -> {m with state = Idle}
    | Play, WaitWork t -> {m with state = Work t}
    | Play, WaitRest t -> {m with state = Rest t}

    | Tick d_t, Work t when t > d_t      -> {m with state = Work (t - d_t)}
    | Tick _,   Work _                   -> {m with state = WaitRest (if m.cycle >= 2
                                                                      then m.def.long_break
                                                                      else m.def.break)}
                                            |> tap send_notification
    | Tick d_t, Rest t when t > d_t     -> {m with state = Rest (t - d_t)}
    | Tick _,   Rest _ when m.cycle < 2 -> {m with state = WaitWork m.def.work;
                                                   cycle = m.cycle + 1}
                                           |> tap send_notification
    | Tick _,   Rest _                  -> {m with state = Finished}
    | Tick _,   _                       -> m

    | Next, Setting i -> {m with state = Setting ((i + 1) mod (Array.length timer_defs))}
    | Next, _         -> m
end
