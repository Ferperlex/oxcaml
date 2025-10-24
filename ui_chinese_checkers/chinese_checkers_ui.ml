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
    let min_x = List.min_elt xs ~compare:Float.compare |> Option.value ~default:0. in
    let max_x = List.max_elt xs ~compare:Float.compare |> Option.value ~default:1. in
    let min_y = List.min_elt ys ~compare:Float.compare |> Option.value ~default:0. in
    let max_y = List.max_elt ys ~compare:Float.compare |> Option.value ~default:1. in
    (* expand half-hex so outer rims are fully visible *)
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

(* Given a selected start cell, collect all legal moves that start there *)
(* let moves_from (st : Game_state.t) (start : Cell_position.t) : Move.t list =
  Game_state.all_legal_moves st
  |> List.filter ~f:(function
    | s :: _ -> pos_equal s start
    | _ -> false)
;; *)

(* ---------- Bonsai UI ---------- *)
module Ui = struct
  type model =
    { game_state : Game_state.t
    ; path : Cell_position.t list (* in-progress path; [] means none *)
    }
  [@@deriving equal, sexp]

  let initial_model game_state = { game_state; path = [] }

  type action =
    | Select_start of Cell_position.t
    | Extend_path of Cell_position.t
    | Confirm_path
    | Cancel_path

  let apply_action (m : model) (a : action) : model =
    match a with
    | Cancel_path -> { m with path = [] }
    | Select_start pos ->
      (match m.game_state.decision with
       | Decision.In_progress { whose_turn } ->
         (match Map.find m.game_state.board pos with
          | Some (Some who) when Player_kind.equal who whose_turn && List.is_empty m.path
            -> { m with path = [ pos ] }
          | _ -> m)
       | _ -> m)
    | Extend_path dst ->
      if List.is_empty m.path then m else { m with path = m.path @ [ dst ] }
    | Confirm_path ->
      if List.length m.path < 2
      then m
      else (
        match Game_state.is_move_valid m.game_state m.path with
        | Ok () ->
          (match Game_state.make_move m.game_state m.path with
           | Ok st' -> { game_state = st'; path = [] }
           | Error _ -> { m with path = [] })
        | Error _ -> { m with path = [] })
  ;;

  let view (model : model) ~(inject : action -> unit Ui_effect.t) : Vdom.Node.t =
    let st = model.game_state in
    (* Precompute layout *)
    let cells = Map.to_alist st.board in
    let bbox = Layout.bbox_of_cells cells in
    let cell_w, cell_h = Layout.cell_size_pct ~bbox in
    (* Compute next steps based on staged path *)
    let next_steps = Game_state.next_steps_from_path st ~path:model.path in
    let is_next pos = List.mem next_steps pos ~equal:pos_equal in
    (* Show moving overlay at head of staged path *)
    let moving_owner, moving_head =
      match model.path with
      | start :: _ ->
        (match Map.find st.board start with
         | Some (Some who) -> Some who, List.last model.path
         | _ -> None, None)
      | [] -> None, None
    in
    (* Confirm becomes active only if path is a legal move *)
    let can_confirm =
      if List.length model.path >= 2
      then (
        match Game_state.is_move_valid st model.path with
        | Ok () -> true
        | Error _ -> false)
      else false
    in
    (* Render a single hex cell *)
    let render_cell ((pos : Cell_position.t), occ) =
      let x, y = Layout.xy_of_axial (pos.q_coordinate, pos.r_coordinate) in
      let left_pct, top_pct = Layout.normalize ~bbox (x, y) in
      let alt = (pos.q_coordinate + pos.r_coordinate) land 1 = 0 in
      (* Hide base piece if it’s the start of a staged path *)
      let is_path_start =
        match model.path with
        | s :: _ -> pos_equal s pos
        | _ -> false
      in
      (* Base (board) piece; selectable if it's your turn and no path in progress *)
      let base_piece =
        match occ with
        | None -> Vdom.Node.none
        | Some who ->
          let selectable =
            match st.decision, model.path with
            | Decision.In_progress { whose_turn }, []
              when Player_kind.equal whose_turn who ->
              [ Vdom.Attr.on_click (fun _ -> inject (Select_start pos)) ]
            | _ -> []
          in
          if is_path_start
          then Vdom.Node.none
          else
            Vdom.Node.div
              ~attrs:
                (Vdom.Attr.classes [ "piece"; "piece--" ^ class_of_player who ]
                 :: selectable)
              []
      in
      (* Moving overlay piece at the head of the path (speculative move) *)
      let moving_piece =
        match moving_owner, moving_head with
        | Some who, Some head when pos_equal pos head ->
          Vdom.Node.div
            ~attrs:
              [ Vdom.Attr.classes
                  [ "piece"; "piece--moving"; "piece--" ^ class_of_player who ]
              ]
            []
        | _ -> Vdom.Node.none
      in
      (* Click: extend path to a next-step destination *)
      let dest_click_attr =
        if is_next pos
        then Vdom.Attr.on_click (fun _ -> inject (Extend_path pos))
        else Vdom.Attr.empty
      in
      let cell_classes =
        [ "cell" ]
        @ (if alt then [ "cell--alt" ] else [])
        @ if is_next pos then [ "cell--next" ] else []
      in
      let style =
        Css_gen.(
          left (`Percent (Percent.of_percentage left_pct))
          @> top (`Percent (Percent.of_percentage top_pct))
          @> width (`Percent (Percent.of_percentage cell_w))
          @> height (`Percent (Percent.of_percentage cell_h)))
      in
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.classes cell_classes; Vdom.Attr.style style; dest_click_attr ]
        [ base_piece; moving_piece ]
    in
    (* HUD with Confirm/Cancel while building a move *)
    let hud =
      match model.path with
      | [] -> Vdom.Node.none
      | _ :: _ ->
        Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "hud" ]
          [ Vdom.Node.div
              ~attrs:[ Vdom.Attr.class_ "hud__actions" ]
              [ Vdom.Node.button
                  ~attrs:
                    ([ Vdom.Attr.class_ "btn btn--confirm" ]
                     @
                     if can_confirm
                     then [ Vdom.Attr.on_click (fun _ -> inject Confirm_path) ]
                     else [ Vdom.Attr.create "disabled" "" ])
                  [ Vdom.Node.text "Confirm" ]
              ; Vdom.Node.button
                  ~attrs:
                    [ Vdom.Attr.class_ "btn btn--cancel"
                    ; Vdom.Attr.on_click (fun _ -> inject Cancel_path)
                    ]
                  [ Vdom.Node.text "Cancel" ]
              ]
          ]
    in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "game" ]
      [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "board" ] (List.map cells ~f:render_cell)
      ; hud
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

(* Entry point *)
let app =
  let initial_state = Game_state.create ~number_of_players:2 |> Or_error.ok_exn in
  Ui.component ~initial_state
;;

let () = Bonsai_web.Start.start app
