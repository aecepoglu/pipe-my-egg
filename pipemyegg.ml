type timer_definition = {
  work: int;
  break: int;
  long_break: int;
}

let timer_defs = [|
  {work = 25 * 60; break = 5 * 60; long_break = 30 * 60};
  {work = 50 * 60; break = 10 * 60; long_break = 60 * 60};
  {work = 15 * 60; break = 5 * 60; long_break = 15 * 60};
  {work = 15; break = 5; long_break = 10};
|]

let string_of_timerdef x = Printf.sprintf "%d/%d/%d"
                             (x.work / 60) (x.break / 60) (x.long_break / 60)

let timestring_of_seconds x = Printf.sprintf "%dm" ((x / 60) + 1)

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
    match String.split_on_char ' ' str with
    | ["play"] -> Ok Play
    | ["next"] -> Ok Next
    | h :: _ -> parsing_err "'play' | 'next'" (spf "'%s' isnt't a recognized command" h)
    | [] -> Error "Empty message"

let open_fifo path =
  (if Sys.file_exists path then
     Sys.remove path
   else
     ()
  );
  let perm = 0o640 in
    Unix.mkfifo path perm;
    Unix.openfile path [Unix.O_RDWR] perm

let read_fifo f fd =
  let bufsize = 16 in
  let buf = Bytes.create bufsize in
    while true; do
      let c = Unix.read fd buf 0 bufsize in
        f (Bytes.sub_string buf 0 (c - 1))
    done

let () =
  let app = new Uiapp.application ({
      state = Idle;
      cycle = 0;
      def = timer_defs.(0);
    }:Pomodoro.model) Pomodoro.update Pomodoro.view in
    app#render ();
  let _thread = Thread.create (fun _ ->
      let t = 20 in
      while true; do
        Unix.sleep t;
        app#process (Tick t)
      done
    ) () in
  open_fifo "/tmp/pipe2egg.sock"
  |> read_fifo (fun str -> msg_of_string str
                           |> (function
                               | Ok u -> app#process u
                               | Error e -> print_endline e
                             )
               )
