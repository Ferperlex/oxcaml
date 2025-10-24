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
    (* flat-topped axial to pixel *)
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
    let min_x = List.min_elt xs ~compare:Float.compare |> Option.value ~default:0. in
    let max_x = List.max_elt xs ~compare:Float.compare |> Option.value ~default:1. in
    let min_y = List.min_elt ys ~compare:Float.compare |> Option.value ~default:0. in
    let max_y = List.max_elt ys ~compare:Float.compare |> Option.value ~default:1. in
    (* expand by half hex so the outer cells fit fully *)
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
(* let last_exn xs = Option.value_exn (List.last xs) *)

let class_of_player = function
  | Player_kind.A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"
  | E -> "E"
  | F -> "F"
;;

let classes xs = Vdom.Attr.class_ (String.concat ~sep:" " xs)

(* ---------- Bonsai UI ---------- *)
module Ui = struct
  type model =
    { game_state : Game_state.t
    ; selection : Cell_position.t option
    ; path : Cell_position.t list (* move being constructed; [] = none *)
    }
  [@@deriving equal, sexp]

  let initial_model game_state = { game_state; selection = None; path = [] }

  type action =
    | Click_cell of Cell_position.t
    | Confirm_move
    | Cancel_move

  let rec auto_skip_if_needed (st : Game_state.t) : Game_state.t =
    match st.decision with
    | Decision.Winner _ -> st
    | Decision.In_progress _ ->
      if Game_state.has_any_legal_moves st
      then st
      else auto_skip_if_needed (Game_state.skip_turn st)
  ;;

  let apply_action (model : model) (action : action) : model =
    let st = model.game_state in
    match action with
    | Cancel_move -> { model with selection = None; path = [] }
    | Confirm_move ->
      (match model.path with
       | _ :: _ :: _ as mv ->
         (match Game_state.make_move model.game_state mv with
          | Error _ ->
            (* illegal => just reset; user can try again *)
            { model with selection = None; path = [] }
          | Ok st' ->
            let st'' = auto_skip_if_needed st' in
            { game_state = st''; selection = None; path = [] })
       | _ ->
         (* nothing to confirm *)
         model)
    | Click_cell pos ->
      (* Helper to compute legal next-steps for the current path *)
      let legal_next () =
        match model.path with
        | [] -> []
        | path -> Game_state.next_steps_from_path st ~path
      in
      (match model.selection, model.path with
       | None, _ ->
         (* select a piece if it belongs to side-to-move *)
         (match Map.find st.board pos, st.decision with
          | Some (Some who), Decision.In_progress { whose_turn }
            when Player_kind.equal who whose_turn ->
            { model with selection = Some pos; path = [ pos ] }
          | _ -> model)
       | Some start, (_ :: _ as path) ->
         (* If clicked one of the legal next-steps, extend the path *)
         if List.mem (legal_next ()) pos ~equal:pos_equal
         then { model with path = path @ [ pos ] }
         else if pos_equal pos start
         then
           (* Re-click on start toggles cancel *)
           { model with selection = None; path = [] }
         else (
           (* Clicking a different own piece restarts selection, otherwise ignore *)
           match Map.find st.board pos, st.decision with
           | Some (Some who), Decision.In_progress { whose_turn }
             when Player_kind.equal who whose_turn ->
             { model with selection = Some pos; path = [ pos ] }
           | _ -> model)
       | Some _, [] ->
         (* Shouldn't happen: selection with empty path *)
         { model with selection = None; path = [] })
  ;;

  let view (model : model) ~(inject : action -> unit Ui_effect.t) : Vdom.Node.t =
    let st = model.game_state in
    let cells = Map.to_alist st.board in
    let bbox = Layout.bbox_of_cells cells in
    let cell_w, cell_h = Layout.cell_size_pct ~bbox in
    (* Legal NEXT steps for the in-progress path *)
    let legal_nexts =
      match model.path with
      | [] -> []
      | path -> Game_state.next_steps_from_path st ~path
    in
    (* Build overlay classes for start/goal zones across all players *)
    let start_goal_classes (pos : Cell_position.t) =
      let per_p (p : Player_kind.t) =
        let start_zone =
          Game_state.start_triangle_for_player ~number_of_players:st.number_of_players p
        in
        let in_start = List.mem start_zone pos ~equal:pos_equal in
        let in_goal = List.mem (Game_state.goals_for st p) pos ~equal:pos_equal in
        (if in_start then [ "cell--start-" ^ class_of_player p ] else [])
        @ if in_goal then [ "cell--goal-" ^ class_of_player p ] else []
      in
      Game_state.players_in_game st.number_of_players |> List.concat_map ~f:per_p
    in
    (* Render a single hex cell + optional piece *)
    let render_cell ((pos : Cell_position.t), occ) =
      let x, y = Layout.xy_of_axial (pos.q_coordinate, pos.r_coordinate) in
      let left_pct, top_pct = Layout.normalize ~bbox (x, y) in
      let alt = (pos.q_coordinate + pos.r_coordinate) land 1 = 0 in
      (* Piece node if occupied *)
      let piece =
        match occ with
        | None -> Vdom.Node.none
        | Some who ->
          let is_turn =
            match st.decision with
            | Decision.In_progress { whose_turn } -> Player_kind.equal whose_turn who
            | Decision.Winner _ -> false
          in
          let is_selected =
            match model.selection with
            | Some s -> pos_equal s pos
            | None -> false
          in
          let piece_classes =
            [ "piece"; "piece--" ^ class_of_player who ]
            @ (if is_turn then [ "piece--clickable" ] else [])
            @ if is_selected then [ "piece--selected" ] else []
          in
          let click_attr =
            if is_turn
            then Vdom.Attr.on_click (fun _ -> inject (Click_cell pos))
            else Vdom.Attr.empty
          in
          Vdom.Node.div ~attrs:[ classes piece_classes; click_attr ] []
      in
      (* If this cell is a legal next step for current path, clicking it extends the path *)
      let dest_click_attr =
        if List.mem legal_nexts pos ~equal:pos_equal
        then Vdom.Attr.on_click (fun _ -> inject (Click_cell pos))
        else Vdom.Attr.empty
      in
      let zone = start_goal_classes pos in
      let cell_classes =
        [ "cell"
        ; (if alt then "cell--alt" else "")
        ; (if List.mem legal_nexts pos ~equal:pos_equal then "cell--legal" else "")
        ]
        @ zone
      in
      let style =
        Css_gen.(
          left (`Percent (Percent.of_percentage left_pct))
          @> top (`Percent (Percent.of_percentage top_pct))
          @> width (`Percent (Percent.of_percentage cell_w))
          @> height (`Percent (Percent.of_percentage cell_h)))
      in
      Vdom.Node.div
        ~attrs:[ classes cell_classes; Vdom.Attr.style style; dest_click_attr ]
        [ piece ]
    in
    (* HUD: Confirm / Cancel when a move path has at least two positions *)
    let hud =
      let can_confirm =
        match model.path with
        | _ :: _ :: _ ->
          (match Game_state.is_move_valid st model.path with
           | Ok () -> true
           | Error _ -> false)
        | _ -> false
      in
      let confirm_btn =
        Vdom.Node.button
          ~attrs:
            [ Vdom.Attr.class_ "btn btn--confirm"
            ; (if can_confirm
               then Vdom.Attr.on_click (fun _ -> inject Confirm_move)
               else Vdom.Attr.create "disabled" "")
            ]
          [ Vdom.Node.text "Confirm" ]
      in
      let cancel_btn =
        Vdom.Node.button
          ~attrs:
            [ Vdom.Attr.class_ "btn btn--cancel"
            ; Vdom.Attr.on_click (fun _ -> inject Cancel_move)
            ]
          [ Vdom.Node.text "Cancel" ]
      in
      (* Only show when there's an active path (>=1 step). Cancel should always appear once selected *)
      match model.path with
      | [] | [ _ ] -> Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "hud" ] []
      | _ -> Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "hud" ] [ confirm_btn; cancel_btn ]
    in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "game" ]
      [ hud
      ; Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "board" ] (List.map cells ~f:render_cell)
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
    let inject a =
      let next = apply_action model a in
      set_model next
    in
    view model ~inject
  ;;
end

(* Entry point used by Bonsai_web.Start.start *)
let app =
  let initial_state =
    Game_state.create ~number_of_players:2 |> Or_error.ok_exn |> Ui.auto_skip_if_needed
  in
  Ui.component ~initial_state
;;

let () = Bonsai_web.Start.start app
