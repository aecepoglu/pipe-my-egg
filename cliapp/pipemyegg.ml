open Mylib.Pomodoro

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

let send_notification (summary:string) :unit =
  Notification.notify ~summary ~timeout:0 ()
  |> Lwt_main.run
  |> ignore

let update_msg_of_string str =
  let parsing_err usage errmsg = Error (Printf.sprintf "Error parsing message '%s'. %s. Correct usage: %s"
                                          str
                                          errmsg
                                          usage
                                       ) in
    match String.split_on_char ' ' str with
    | ["play"] -> Ok Play
    | ["next"] -> Ok Next
    | h :: _ -> parsing_err "'play' | 'next'" (Printf.sprintf "'%s' isnt't a recognized command" h)
    | [] -> Error "Empty message"

let view (model:Pomodoro.model) =
  let s = (
    match model.state with
    | Idle               -> "IDL"
    | Setting i          -> string_of_timerdef timer_defs.(i)
    | WaitWork i         -> "w " ^ timestring_of_seconds i
    | Work i             -> "W " ^ timestring_of_seconds i
    | Rest i             -> "R " ^ timestring_of_seconds i
    | WaitRest i         -> "r " ^ timestring_of_seconds i
    | Finished           -> "FIN"
  ) in
    print_endline s

let () =
  let open Uiapp in
  let app = App.init
              ({state = Idle;
                cycle = 0;
                def = timer_defs.(0);
               }: Pomodoro.model)
              (Pomodoro.update ~notify_state_change:send_notification)
              view in
    view app.model;
    let _thread = Thread.create (fun _ ->
        let t = 20 in
          while true; do
            Unix.sleep t;
            App.process_event app (Tick t)
          done
      ) () in
      open_fifo "/tmp/pipe2egg.sock"
      |> read_fifo (fun str -> update_msg_of_string str
                               |> (function
                                   | Ok u -> App.process_event app u
                                   | Error e -> print_endline e
                                 )
                   )
