type timer_definition = {
  work: int;
  break: int;
  long_break: int;
}

let timer_defs = [|
  {work = 25 * 60; break = 5 * 60; long_break = 30 * 60};
  {work = 50 * 60; break = 10 * 60; long_break = 60 * 60};
  {work = 15 * 60; break = 5 * 60; long_break = 15 * 60};
  {work = 2 * 60; break = 1 * 60; long_break = 3 * 60};
|]

let string_of_timerdef x = Printf.sprintf "%d/%d/%d" (x.work / 60) (x.break / 60) (x.long_break / 60)

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
    | WaitLongRest of int
    | LongRest of int
    | Finished

  type model = {
    state: state;
    cycle: int;
    def: timer_definition;
  }

  let update m msg = match msg, m with
    | Play, {state = Idle; _}                        -> {m with state = Setting 0}
    | Play, {state = Setting i; _}                   -> let def = timer_defs.(i) in {def;
                                                                                     state = Work def.work;
                                                                                     cycle = 0}
    | Play, {state = Work _; _}
    | Play, {state = Rest _; _}
    | Play, {state = LongRest _; _}                  -> {m with state = Finished}
    | Play, {state = Finished; _}                    -> {m with state = Idle}
    | Play, {state = WaitWork x; _}                  -> {m with state = WaitWork x}
    | Play, {state = WaitRest x; _}                  -> {m with state = Rest x}
    | Play, {state = WaitLongRest x; _}              -> {m with state = LongRest x}

    | Tick d_i, {state = Work i; _} when i > d_i     -> {m with state = Work (i - d_i)}
    | Tick _,   {state = Work _; cycle = 2; _}       -> {m with state = WaitLongRest m.def.long_break}
    | Tick _,   {state = Work _; cycle; _}           -> {m with state = WaitRest m.def.break;
                                                                cycle = cycle + 1}
    | Tick d_i, {state = Rest i; _} when i > d_i     -> {m with state = Rest (i - 1)}
    | Tick _,   {state = Rest _; _}                  -> {m with state = WaitWork m.def.work}
    | Tick d_i, {state = LongRest i; _} when i > d_i -> {m with state = LongRest (i - 1)}
    | Tick _,   {state = LongRest _; _}              -> {m with state = Finished}
    | Tick _,   _                                    -> m

    | DefineTimer def', _                            -> {m with def = def'} (* TODO get rid of this message *)

    | Next, {state = Setting i; _}                   -> {m with state = Setting ((i + 1) mod (Array.length timer_defs))}
    | Next, _                                        -> m

  let view {state; cycle; def} =
    let s = (
      match state with
      | Idle               -> "IDL"
      | Setting i          -> string_of_timerdef timer_defs.(i)
      | WaitWork i         -> "w " ^ string_of_int (i)
      | Work i             -> "W " ^ string_of_int (i)
      | Rest i
      | LongRest i         -> "R " ^ string_of_int (i)
      | WaitRest i
      | WaitLongRest i     -> "r " ^ string_of_int (i)
      | Finished           -> Printf.sprintf "FIN %dm" (cycle * def.work)
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
  let _thread = Thread.create (fun _ ->
      while true; do
        Unix.sleep 1;
        app#process (Tick 1)
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
