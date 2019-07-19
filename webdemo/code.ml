open Mylib.Pomodoro

let string_of_seconds x = Printf.sprintf "%d:%02d" (x / 60) (x mod 60)

let string_of_timerdef (x:Mylib.Pomodoro.timer_definition) = Printf.sprintf "%s/%s/%s"
                                                               (string_of_seconds x.work)
                                                               (string_of_seconds x.break)
                                                               (string_of_seconds x.long_break)

let view (model:Pomodoro.model) =
  let open Vdom in
  let button ?(attrs=[]) txt msg = input [] ~a:([ type_button;
                                                  onclick (fun _ -> msg);
                                                  value txt] @ attrs) in
  let str = (match model.state with
      | Idle -> "Idle"
      | Setting i -> string_of_timerdef (timer_defs.(i))
      | WaitWork _ -> "'OK' to start working"
      | Work i -> "Working " ^ (string_of_seconds i)
      | WaitRest _ -> "'OK' to start your break"
      | Rest i -> "Resting " ^ (string_of_seconds i)
      | Finished -> "Finished"
    )
  in
  div ~a:[attr "id" "app"] [
    div ~a:[attr "id" "display"] [text str];
    div [
      (text "Cycle: ");
      (text (string_of_int (1 + model.cycle)));
      (text "/3");
    ];
    button "OK" Play;
    button ~attrs:[disabled (match model.state with Setting _ -> false | _ -> true)] "Next" Next
  ]

let () =
  let open Js_browser in
  let app = Vdom.simple_app
              ~init:({state = Idle;
                      cycle = 0;
                      def = timer_defs.(0);
                     }: Pomodoro.model)
              ~update:Pomodoro.update
              ~view:view
              ()
  in
  let run () =
    Vdom_blit.run app
    |> (fun app' ->
        let _ = Window.set_interval
                  window
                  (fun () -> Vdom_blit.process app' (Tick 1))
                  100 in
          app'
      )
    |> Vdom_blit.dom
    |> Element.append_child (match Document.get_element_by_id document "container" with
      | Some elem -> elem
      | None -> Document.body document
      )
  in
    Window.set_onload window run

