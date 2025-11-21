open Core
open Async_kernel
open Incr_dom

module App = struct
  module Model = struct
    type t = { board : Engine.Board.t; width : int; height : int }
    [@@deriving fields, compare]

    let cutoff = phys_equal
  end

  module Action = struct
    type t = Tick | Resize of int * int | Click of int * int [@@deriving sexp]
  end

  (* State is for holding imperitive state. e.g.  connections to servers.
     we aren't making use of it but it is required by Start_app.start *)
  module State = struct
    type t = unit
  end

  let initial_model =

    let glider_gun =
      Patterns.glider_gun
      |> List.map ~f:(fun (x, y) -> (x + 1, y + 1))
      (* shift glider gun by a bit *)
    in

    let starting_board =
      Engine.Board.empty |> Engine.Board.add_cells glider_gun
    in
    
    let open Js_of_ocaml in
    {
      Model.board = starting_board;
      width = Dom_html.window##.innerWidth;
      height = Dom_html.window##.innerHeight;
    }

  let on_startup ~schedule_action _model =
    let open Js_of_ocaml in

    (* Tick every 0.25s -- TODO: make async *)
    every (Time_ns.Span.of_sec 0.25) (fun () -> schedule_action Action.Tick);

    (* Listen for window resizing *)
    Dom_html.window##.onresize :=
      Dom_html.handler (fun _ ->
          let w = Dom_html.window##.innerWidth in
          let h = Dom_html.window##.innerHeight in
          schedule_action (Action.Resize (w, h));
          Js._true);

    Deferred.unit

  let create model ~old_model:_ ~(inject : Action.t -> unit Vdom.Effect.t) =
    let open Incr.Let_syntax in
    let%map apply_action =
      let%map board = model >>| Model.board
      and width = model >>| Model.width
      and height = model >>| Model.height in
      fun (action : Action.t) _ ~schedule_action:_ ->
        match action with
        | Tick -> { Model.board = Engine.Board.step board; width; height }
        | Resize (w, h) -> { Model.board; width = w; height = h }
        | Click (x, y) ->
            let new_board =
              if Engine.Board.mem (x, y) board then
                Engine.Board.remove (x, y) board
              else Engine.Board.add (x, y) board
            in
            { Model.board = new_board; width; height }
    and view =
      (* on the LHS of let%map board is a constant value; while on the RHS the board fetched from the model
         is an incremental value *)
      let%map board = model >>| Model.board
      and width = model >>| Model.width
      and height = model >>| Model.height in

      let cell_size = 24 in

      let cell x y =
        let colour =
          if Engine.Board.mem (x, y) board then "tomato" else "white"
        in
        let style =
          let ( @> ) = Css_gen.( @> ) in
          Css_gen.background_color (`Name colour)
          @> Css_gen.width (`Px cell_size)
          @> Css_gen.height (`Px cell_size)
        in

        Vdom.Node.td
          [ Vdom.Node.text "" ]
          ~attrs:
            [
              Vdom.Attr.style style;
              Vdom.Attr.on_click (fun _ -> inject (Click (x, y)));
            ]
      in

      let cols = width / cell_size in
      let rows = height / cell_size in

      let row y =
        let cells = List.init cols ~f:(fun x -> cell x y) in
        Vdom.Node.tr cells
      in

      Vdom.Node.div [ Vdom.Node.table (List.init rows ~f:row) ]
    and model = model in

    Component.create ~apply_action model view
end

let () =
  Start_app.start
    (module App)
    ~bind_to_element_with_id:"app" ~initial_model:App.initial_model
