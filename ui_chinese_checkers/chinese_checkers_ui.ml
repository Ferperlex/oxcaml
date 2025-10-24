open! Core
open Virtual_dom
open Chinese_checkers_logic_library
open Game
open! Bonsai.Let_syntax

(* ---------- Hex layout (flat-topped axial -> normalized %) ---------- *)
module Layout = struct
  let size = 1.0
  let w = 2.0 *. size
  let h = Float.sqrt 3.0 *. size
  let half_w = w /. 2.0
  let half_h = h /. 2.0

  let xy_of_axial (q, r) =
    let qf = Float.of_int q
    and rf = Float.of_int r in
    let x = 1.5 *. size *. qf in
    let y = Float.sqrt 3.0 *. size *. (rf +. (qf /. 2.0)) in
    x, y
  ;;

  type bbox =
    { min_x : float
    ; max_x : float
    ; min_y : float
    ; max_y : float
    }

  let bbox_of_cells (cells : (Cell_position.t * 'a) list) : bbox =
    let xs, ys =
      List.map cells ~f:(fun (pos, _) -> xy_of_axial (pos.q_coordinate, pos.r_coordinate))
      |> List.unzip
    in
    let min_x = Option.value (List.min_elt xs ~compare:Float.compare) ~default:0. in
    let max_x = Option.value (List.max_elt xs ~compare:Float.compare) ~default:1. in
    let min_y = Option.value (List.min_elt ys ~compare:Float.compare) ~default:0. in
    let max_y = Option.value (List.max_elt ys ~compare:Float.compare) ~default:1. in
    { min_x = min_x -. half_w
    ; max_x = max_x +. half_w
    ; min_y = min_y -. half_h
    ; max_y = max_y +. half_h
    }
  ;;

  let normalize ~bbox (x, y) =
    let nx = (x -. bbox.min_x) /. (bbox.max_x -. bbox.min_x) in
    let ny = (y -. bbox.min_y) /. (bbox.max_y -. bbox.min_y) in
    nx *. 100.0, ny *. 100.0
  ;;

  let cell_size_pct ~bbox =
    let cw = w /. (bbox.max_x -. bbox.min_x) *. 100.0 in
    let ch = h /. (bbox.max_y -. bbox.min_y) *. 100.0 in
    cw, ch
  ;;
end

(* ---------- Small helpers ---------- *)
let pos_equal a b = Int.equal (Cell_position.compare a b) 0

let class_of_player = function
  | Player_kind.A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"
  | E -> "E"
  | F -> "F"
;;

(* ---------- Bonsai UI ---------- *)
module Ui = struct
  type model =
    { game_state : Game_state.t
    ; path : Cell_position.t list (* [] = no selection; [start; ...steps...] otherwise *)
    }
  [@@deriving equal, sexp]

  let initial_model game_state = { game_state; path = [] }

  type action =
    | Select_start of Cell_position.t
    | Add_step of Cell_position.t
    | Confirm_path
    | Cancel_path

  let apply_action (model : model) (action : action) : model =
    let st = model.game_state in
    match action with
    | Cancel_path -> { model with path = [] }
    | Select_start pos ->
      (* Only allow selecting a piece that belongs to the current player *)
      (match st.decision with
       | Decision.Winner _ -> model
       | Decision.In_progress { whose_turn } ->
         (match Map.find st.board pos with
          | Some (Some who) when Player_kind.equal who whose_turn ->
            { model with path = [ pos ] }
          | _ -> model))
    | Add_step dst ->
      (match model.path with
       | [] -> model
       | start :: _ ->
         let { Game_state.adjacents; hops } =
           Game_state.next_step_options st ~start ~path:model.path
         in
         if List.mem adjacents dst ~equal:pos_equal || List.mem hops dst ~equal:pos_equal
         then { model with path = model.path @ [ dst ] }
         else model)
    | Confirm_path ->
      (match model.path with
       | mv when List.length mv >= 2 ->
         (match Game_state.is_move_valid st mv, Game_state.make_move st mv with
          | Ok (), Ok st' -> { game_state = st'; path = [] }
          | _ -> { model with path = [] })
       | _ -> model)
  ;;

  let view (model : model) ~(inject : action -> unit Ui_effect.t) : Vdom.Node.t =
    let st = model.game_state in
    let cells = Map.to_alist st.board in
    let bbox = Layout.bbox_of_cells cells in
    let cell_w, cell_h = Layout.cell_size_pct ~bbox in
    (* compute step options for current path *)
    let next_opts =
      match model.path with
      | [] -> None
      | start :: _ -> Some (Game_state.next_step_options st ~start ~path:model.path)
    in
    let is_legal_next pos =
      match next_opts with
      | None -> false
      | Some { Game_state.adjacents; hops } ->
        List.mem adjacents pos ~equal:pos_equal || List.mem hops pos ~equal:pos_equal
    in
    (* Render a single cell (as circular “slot”) and a piece if present *)
    let render_cell ((pos : Cell_position.t), occ) =
      let x, y = Layout.xy_of_axial (pos.q_coordinate, pos.r_coordinate) in
      let left_pct, top_pct = Layout.normalize ~bbox (x, y) in
      let zone_classes =
        let per_p p =
          let in_start =
            List.mem
              (Game_state.start_triangle_for_player
                 ~number_of_players:st.number_of_players
                 p)
              pos
              ~equal:pos_equal
          in
          let in_goal = List.mem (Game_state.goals_for st p) pos ~equal:pos_equal in
          (if in_start then [ "cell--start-" ^ class_of_player p ] else [])
          @ if in_goal then [ "cell--goal-" ^ class_of_player p ] else []
        in
        Game_state.players_in_game st.number_of_players |> List.concat_map ~f:per_p
      in
      (* circular slot we click on to extend the path *)
      let slot_classes =
        [ "slot" ] @ zone_classes @ if is_legal_next pos then [ "slot--legal" ] else []
      in
      let slot_click =
        if is_legal_next pos
        then Vdom.Attr.on_click (fun _ -> inject (Add_step pos))
        else Vdom.Attr.empty
      in
      let style =
        Css_gen.(
          left (`Percent (Percent.of_percentage left_pct))
          @> top (`Percent (Percent.of_percentage top_pct))
          @> width (`Percent (Percent.of_percentage cell_w))
          @> height (`Percent (Percent.of_percentage cell_h)))
      in
      (* piece node (stays at its real board position until confirm) *)
      let piece =
        match occ with
        | None -> Vdom.Node.none
        | Some who ->
          let highlighted =
            match model.path with
            | start :: _ when pos_equal start pos -> [ "piece--selected" ]
            | _ -> []
          in
          Vdom.Node.div
            ~attrs:
              [ Vdom.Attr.classes
                  ([ "piece"; "piece--" ^ class_of_player who ] @ highlighted)
              ]
            []
      in
      (* allow selecting your own piece by clicking the piece itself *)
      let piece_select_attr =
        match st.decision, occ with
        | Decision.In_progress { whose_turn }, Some who
          when Player_kind.equal who whose_turn ->
          Vdom.Attr.on_click (fun _ -> inject (Select_start pos))
        | _ -> Vdom.Attr.empty
      in
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "cell"; Vdom.Attr.style style ]
        [ Vdom.Node.div ~attrs:[ Vdom.Attr.classes slot_classes; slot_click ] []
        ; Vdom.Node.div ~attrs:[ piece_select_attr ] [ piece ]
        ]
    in
    (* confirm/cancel bar appears only if we have at least one step chosen *)
    let show_confirm =
      match model.path with
      | _ :: _ :: _ -> true
      | _ -> false
    in
    let confirm_bar =
      if show_confirm
      then
        Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "confirm-bar" ]
          [ Vdom.Node.button
              ~attrs:
                [ Vdom.Attr.classes [ "btn"; "btn--confirm" ]
                ; Vdom.Attr.on_click (fun _ -> inject Confirm_path)
                ]
              [ Vdom.Node.text "Confirm move" ]
          ; Vdom.Node.button
              ~attrs:
                [ Vdom.Attr.classes [ "btn"; "btn--reset" ]
                ; Vdom.Attr.on_click (fun _ -> inject Cancel_path)
                ]
              [ Vdom.Node.text "Cancel" ]
          ]
      else Vdom.Node.none
    in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "game" ]
      [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "board" ] (List.map cells ~f:render_cell)
      ; confirm_bar
      ]
  ;;

  let component ~(initial_state : Game_state.t) =
    let%sub model, set_model =
      Bonsai.state
        (module struct
          type t = model [@@deriving equal, sexp]
        end)
        ~default_model:(initial_model initial_state)
    in
    let%arr model = model
    and set_model = set_model in
    let inject a = set_model (apply_action model a) in
    view model ~inject
  ;;
end

let app =
  let initial_state = Game_state.create ~number_of_players:2 |> Or_error.ok_exn in
  Ui.component ~initial_state
;;

let () = Bonsai_web.Start.start app
