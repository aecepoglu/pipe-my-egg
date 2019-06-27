type timer_definition = {
  work: int;
  break: int;
  long_break: int;
}

let timer_defs = [|
  {work = 15; break = 5; long_break = 10};
  {work = 25 * 60; break = 5 * 60; long_break = 30 * 60};
  {work = 50 * 60; break = 10 * 60; long_break = 60 * 60};
  {work = 15 * 60; break = 5 * 60; long_break = 15 * 60};
|]

let string_of_timerdef x = Printf.sprintf "%d/%d/%d"
                             (x.work / 60) (x.break / 60) (x.long_break / 60)

let timestring_of_seconds x = Printf.sprintf "%d:%02d" (x / 60) (x mod 60)

let tap f x = f x; x

type update_msg =
  | Play
  | Tick of int
  | DefineTimer of timer_definition
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

  let notify_state {state; _} =
    let summary = (match state with
        | Idle -> "idle"
        | Setting _ -> "Setting durations"
        | WaitWork _ -> "Waiting to start work"
        | Work _ -> "Work began"
        | Rest _ -> "Rest began"
        | WaitRest _ -> "Waiting to start rest"
        | Finished -> "end"
      ) in
    Notification.notify ~summary ()
    |> Lwt_main.run
    |> ignore

  let update m msg =
    match msg, m.state with
    | Play, Idle      -> {m with state = Setting 0}
    | Play, Setting i -> let def = timer_defs.(i) in {def;
                                                      state = Work def.work;
                                                      cycle = 0}
                                                     |> tap notify_state
    | Play, Work _
    | Play, Rest _
    | Play, Finished   -> {m with state = Idle}
    | Play, WaitWork t -> {m with state = Work t}
    | Play, WaitRest t -> {m with state = Rest t}

    | Tick d_t, Work t when t > d_t      -> {m with state = Work (t - d_t)}
    | Tick _,   Work _                   -> {m with state = WaitRest (if m.cycle >= 2
                                                                      then m.def.long_break
                                                                      else m.def.break)}
                                            |> tap notify_state
    | Tick d_t, Rest t when t > d_t     -> {m with state = Rest (t - d_t)}
    | Tick _,   Rest _ when m.cycle < 2 -> {m with state = WaitWork m.def.work;
                                                   cycle = m.cycle + 1}
                                           |> tap notify_state
    | Tick _,   Rest _                  -> {m with state = Finished}
    | Tick _,   _                       -> m

    | DefineTimer def', _ -> {m with def = def'} (* TODO get rid of this message *)
    | Next, Setting i -> {m with state = Setting ((i + 1) mod (Array.length timer_defs))}
    | Next, _         -> m

  let view {state; _} =
    let s = (
      match state with
      | Idle               -> "IDL"
      | Setting i          -> string_of_timerdef timer_defs.(i)
      | WaitWork i         -> "w " ^ timestring_of_seconds i
      | Work i             -> "W " ^ timestring_of_seconds i
      | Rest i             -> "R " ^ timestring_of_seconds i
      | WaitRest i         -> "r " ^ timestring_of_seconds i
      | Finished           -> "FIN"
    ) in
      print_endline s
end


let msg_of_string str =
  let spf = Printf.sprintf in
  let parsing_err usage errmsg = Error (spf "Error parsing message '%s'. %s. Correct usage: %s"
                                          str
                                          errmsg
                                          usage
                                       ) in
  let def_err  = parsing_err "'def (work) (break) (long break)'" in
  let def_key_err key given = def_err (spf
                                         "'def' message expects '%s' to be a number but '%s' isn't"
                                         key
                                         given
                                      ) in
    match String.split_on_char ' ' str with
    | ["play"] -> Ok Play
    | ["next"] -> Ok Next
    | ["def"; work'; break'; long_break'] ->
       let f = int_of_string_opt in
         (match (f work'), (f break'), (f long_break') with
          | Some work, Some break, Some long_break ->
             Ok (DefineTimer { work = 60 * work;
                               break = 60 * break;
                               long_break = 60 * long_break
                             })
          | None, _, _ -> def_key_err "work" work'
          | _, None, _ -> def_key_err "break" break'
          | _, _, None -> def_key_err "long break" long_break'
         )
    | "def" :: l -> def_err (spf "'def' message must be called with %d arguments but it is given %d" 3 (List.length l - 1))
    | h :: _ -> parsing_err "def (work) (break) (long break) | stop" (spf "'%s' isnt't a recognized command" h)
    | [] -> Error "Empty message"

let open_fifo path =
  (if Sys.file_exists path then
     Sys.remove path
   else
     ()
  );
  Unix.mkfifo path 0o640;
  open_in path

let () =
  let app = new Uiapp.application ({
      state = Idle;
      cycle = 0;
      def = timer_defs.(0);
    }:Pomodoro.model) Pomodoro.update Pomodoro.view in
    app#render ();
  let _thread = Thread.create (fun _ ->
      let t = 1 in
      while true; do
        Unix.sleep t;
        app#process (Tick t)
      done
    ) () in
  let ch = open_fifo "/tmp/pipe2egg.sock" in
  let running = ref true in
    while !running; do
      try
        while true; do
          input_line ch
          |> msg_of_string
          |> (function
              | Ok u -> app#process u
              | Error e -> print_endline e
            )
        done
      with
      | End_of_file -> ()
      | _ -> running := false
    done
