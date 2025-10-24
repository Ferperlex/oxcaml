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
let last_exn xs = Option.value_exn (List.last xs)

let class_of_player = function
  | Player_kind.A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"
  | E -> "E"
  | F -> "F"
;;

let directions : (int * int) list = [ 1, 0; 0, 1; -1, 1; -1, 0; 0, -1; 1, -1 ]

let neighbor (pos : Cell_position.t) (dq, dr) : Cell_position.t =
  { Cell_position.q_coordinate = pos.q_coordinate + dq
  ; r_coordinate = pos.r_coordinate + dr
  }
;;

let landing_empty (board : Player_kind.t option Cell_position.Map.t) key =
  match Map.find board key with
  | Some None -> true
  | _ -> false
;;

let occupied (board : Player_kind.t option Cell_position.Map.t) key =
  match Map.find board key with
  | Some (Some _) -> true
  | _ -> false
;;

let hop_targets
      (st : Game_state.t)
      ~(curr : Cell_position.t)
      ~(visited : Cell_position.t list)
  : Cell_position.t list
  =
  directions
  |> List.filter_map ~f:(fun (dq, dr) ->
    let mid = neighbor curr (dq, dr) in
    let jump = neighbor curr (2 * dq, 2 * dr) in
    if
      occupied st.board mid
      && landing_empty st.board jump
      && not (List.mem visited jump ~equal:pos_equal)
    then Some jump
    else None)
;;

let adjacent_empties (st : Game_state.t) ~(curr : Cell_position.t) : Cell_position.t list =
  directions
  |> List.filter_map ~f:(fun dir ->
    let dst = neighbor curr dir in
    if landing_empty st.board dst then Some dst else None)
;;

let legal_next_positions (st : Game_state.t) (path : Move.t) : Cell_position.t list =
  match path with
  | [] -> []
  | [ start ] ->
    let hops = hop_targets st ~curr:start ~visited:path in
    let steps = adjacent_empties st ~curr:start in
    steps @ hops
  | a :: b :: _ ->
    let dq = b.q_coordinate - a.q_coordinate in
    let dr = b.r_coordinate - a.r_coordinate in
    let is_adjacent = List.mem directions (dq, dr) ~equal:Poly.equal in
    if is_adjacent then [] else hop_targets st ~curr:(last_exn path) ~visited:path
;;

(* Auto-skip until the current player has at least one legal move (or game over). *)
let rec force_turn_with_moves (st : Game_state.t) : Game_state.t =
  match st.decision with
  | Decision.Winner _ -> st
  | Decision.In_progress _ ->
    if Game_state.has_any_legal_moves st
    then st
    else force_turn_with_moves (Game_state.skip_turn st)
;;

(* ---------- Bonsai UI ---------- *)
module Ui = struct
  type model =
    { game_state : Game_state.t
    ; pending_move : Move.t option
    ; last_move : Move.t option
    }
  [@@deriving equal, sexp]

  let initial_model game_state =
    let ready = force_turn_with_moves game_state in
    { game_state = ready; pending_move = None; last_move = None }
  ;;

  type action =
    | Select_start of Cell_position.t
    | Extend_to of Cell_position.t
    | Clear_selection
    | Confirm_move

  let apply_action (model : model) (action : action) : model =
    (* always ensure we’re at a turn with available moves before handling input *)
    let model = { model with game_state = force_turn_with_moves model.game_state } in
    match action with
    | Clear_selection -> { model with pending_move = None }
    | Select_start pos -> { model with pending_move = Some [ pos ] }
    | Extend_to dest ->
      (match model.pending_move with
       | None -> model
       | Some path -> { model with pending_move = Some (path @ [ dest ]) })
    | Confirm_move ->
      (match model.pending_move with
       | None -> model
       | Some mv ->
         (match Game_state.is_move_valid model.game_state mv with
          | Error _ -> model
          | Ok () ->
            (match Game_state.make_move model.game_state mv with
             | Error _ -> model
             | Ok st' ->
               let st'' = force_turn_with_moves st' in
               { game_state = st''; pending_move = None; last_move = Some mv })))
  ;;

  let view (model : model) ~(inject : action -> unit Ui_effect.t) : Vdom.Node.t =
    let st = model.game_state in
    let cells = Map.to_alist st.board in
    let bbox = Layout.bbox_of_cells cells in
    let cell_w, cell_h = Layout.cell_size_pct ~bbox in
    let pending = model.pending_move in
    let next_positions =
      match pending with
      | None -> []
      | Some path -> legal_next_positions st path
    in
    let is_next pos = List.mem next_positions pos ~equal:pos_equal in
    let is_in_path pos =
      match pending with
      | None -> false
      | Some p -> List.mem p pos ~equal:pos_equal
    in
    let path_head =
      match pending with
      | Some (s :: _) -> Some s
      | _ -> None
    in
    let start_goal_classes pos =
      let per_p (p : Player_kind.t) =
        let in_start =
          List.mem
            (Game_state.start_triangle_for_player
               ~number_of_players:st.number_of_players
               p)
            pos
            ~equal:pos_equal
        in
        let in_goal = List.mem (Game_state.goals_for st p) pos ~equal:pos_equal in
        let c1 = if in_start then [ "cell--start-" ^ class_of_player p ] else [] in
        let c2 = if in_goal then [ "cell--goal-" ^ class_of_player p ] else [] in
        c1 @ c2
      in
      Game_state.players_in_game st.number_of_players |> List.concat_map ~f:per_p
    in
    let render_cell ((pos : Cell_position.t), occ) =
      let x, y = Layout.xy_of_axial (pos.q_coordinate, pos.r_coordinate) in
      let left_pct, top_pct = Layout.normalize ~bbox (x, y) in
      let alt = (pos.q_coordinate + pos.r_coordinate) land 1 = 0 in
      let piece =
        match occ with
        | None -> Vdom.Node.none
        | Some who ->
          let selected_head =
            match path_head with
            | Some s when pos_equal s pos -> [ "piece--selected" ]
            | _ -> []
          in
          let classes = [ "piece"; "piece--" ^ class_of_player who ] @ selected_head in
          let selectable =
            match st.decision with
            | Decision.In_progress { whose_turn } when Player_kind.equal whose_turn who ->
              [ Vdom.Attr.on_click (fun _ -> inject (Select_start pos)) ]
            | _ -> []
          in
          Vdom.Node.div ~attrs:(Vdom.Attr.classes classes :: selectable) []
      in
      let dest_click_attr =
        match pending with
        | None -> Vdom.Attr.empty
        | Some _ ->
          if is_next pos
          then Vdom.Attr.on_click (fun _ -> inject (Extend_to pos))
          else Vdom.Attr.empty
      in
      let zone_classes = start_goal_classes pos in
      let cell_classes =
        [ "cell"
        ; (if alt then "cell--alt" else "")
        ; (if is_next pos then "cell--legal" else "")
        ; (if is_in_path pos then "cell--in-path" else "")
        ]
        @ zone_classes
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
        [ piece ]
    in
    let confirm_enabled =
      match pending with
      | Some mv when List.length mv >= 2 ->
        (match Game_state.is_move_valid st mv with
         | Ok () -> true
         | Error _ -> false)
      | _ -> false
    in
    let confirm_btn =
      Vdom.Node.button
        ~attrs:
          [ Vdom.Attr.class_ "btn"
          ; (if confirm_enabled
             then Vdom.Attr.on_click (fun _ -> inject Confirm_move)
             else Vdom.Attr.create "disabled" "")
          ]
        [ Vdom.Node.text "Confirm move" ]
    in
    let cancel_btn =
      Vdom.Node.button
        ~attrs:
          [ Vdom.Attr.class_ "btn"; Vdom.Attr.on_click (fun _ -> inject Clear_selection) ]
        [ Vdom.Node.text "Cancel" ]
    in
    let hud =
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "hud" ]
        [ Vdom.Node.div ~attrs:[ Vdom.Attr.class_ "right" ] [ confirm_btn; cancel_btn ] ]
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

let app =
  let initial_state = Game_state.create ~number_of_players:2 |> Or_error.ok_exn in
  Ui.component ~initial_state
;;

let () = Bonsai_web.Start.start app
