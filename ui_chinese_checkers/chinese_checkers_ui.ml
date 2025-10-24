open! Core
open Virtual_dom
open Chinese_checkers_logic_library
open Game
open! Bonsai.Let_syntax

(* ---------- Hex layout (flat-topped axial -> normalized %) ---------- *)
module Layout = struct
  (* flat-topped, size = 1.0 (we’ll normalize to percentage) *)
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
let last_exn xs = Option.value_exn (List.last xs)

let class_of_player = function
  | Player_kind.A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"
  | E -> "E"
  | F -> "F"
;;

(* Given a selected start cell, collect all legal moves that start there *)
let moves_from (st : Game_state.t) (start : Cell_position.t) : Move.t list =
  Game_state.all_legal_moves st
  |> List.filter ~f:(function
    | s :: _ -> pos_equal s start
    | _ -> false)
;;

(* Prefer the longest hop sequence if multiple moves land on same destination. *)
let pick_move_to_destination (cands : Move.t list) (dest : Cell_position.t)
  : Move.t option
  =
  cands
  |> List.filter ~f:(fun mv -> pos_equal (last_exn mv) dest)
  |> List.max_elt ~compare:(fun a b -> Int.compare (List.length a) (List.length b))
;;

(* ---------- Bonsai UI ---------- *)
module Ui = struct
  type model =
    { game_state : Game_state.t
    ; selection : Cell_position.t option
    ; last_move : Move.t option
    }
  [@@deriving equal, sexp]

  let initial_model game_state = { game_state; selection = None; last_move = None }

  type action =
    | Select_start of Cell_position.t
    | Clear_selection
    | Commit_move of Move.t
    | Skip_turn

  let apply_action (model : model) (action : action) : model =
    match action with
    | Clear_selection -> { model with selection = None }
    | Select_start pos -> { model with selection = Some pos }
    | Skip_turn ->
      (match model.game_state.decision with
       | Decision.Winner _ -> model
       | Decision.In_progress _ ->
         { game_state = Game_state.skip_turn model.game_state
         ; selection = None
         ; last_move = None
         })
    | Commit_move mv ->
      (match Game_state.make_move model.game_state mv with
       | Error _ -> model (* defensive *)
       | Ok st' -> { game_state = st'; selection = None; last_move = Some mv })
  ;;

  let view (model : model) ~(inject : action -> unit Ui_effect.t) : Vdom.Node.t =
    let st = model.game_state in
    (* Precompute layout over all board cells *)
    let cells = Map.to_alist st.board in
    let bbox = Layout.bbox_of_cells cells in
    let cell_w, cell_h = Layout.cell_size_pct ~bbox in
    (* Turn & action hints *)
    (* let whose_turn =
      match st.decision with
      | Decision.In_progress { whose_turn } -> Some whose_turn
      | Decision.Winner _ -> None
    in *)
    let can_skip =
      match st.decision with
      | Decision.Winner _ -> false
      | Decision.In_progress _ -> not (Game_state.has_any_legal_moves st)
    in
    (* selection-dependent legal endpoints *)
    let legal_endpoints =
      match model.selection with
      | None -> []
      | Some start ->
        moves_from st start
        |> List.map ~f:last_exn
        |> List.dedup_and_sort ~compare:Cell_position.compare
    in
    let is_legal_dest pos = List.mem legal_endpoints pos ~equal:pos_equal in
    (* build a class indicating start/goal tint (all players) *)
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
    (* Render a single hex cell *)
    let render_cell ((pos : Cell_position.t), occ) =
      let x, y = Layout.xy_of_axial (pos.q_coordinate, pos.r_coordinate) in
      let left_pct, top_pct = Layout.normalize ~bbox (x, y) in
      let alt = (pos.q_coordinate + pos.r_coordinate) land 1 = 0 in
      (* Piece node if occupied *)
      let piece =
        match occ with
        | None -> Vdom.Node.none
        | Some who ->
          let classes =
            [ "piece"; "piece--" ^ class_of_player who ]
            @
            match model.selection with
            | Some s when pos_equal s pos -> [ "piece--selected" ]
            | _ -> []
          in
          (* clicking your own piece selects it *)
          let selectable =
            match st.decision with
            | Decision.In_progress { whose_turn } when Player_kind.equal whose_turn who ->
              [ Vdom.Attr.on_click (fun _ -> inject (Select_start pos)) ]
            | _ -> []
          in
          Vdom.Node.div ~attrs:(Vdom.Attr.classes classes :: selectable) []
      in
      (* click on a legal destination commits the path (prefer longest hop) *)
      let dest_click_attr =
        match model.selection with
        | None -> Vdom.Attr.empty
        | Some start ->
          if is_legal_dest pos
          then
            Vdom.Attr.on_click (fun _ ->
              match pick_move_to_destination (moves_from st start) pos with
              | None -> Ui_effect.Ignore
              | Some mv -> inject (Commit_move mv))
          else Vdom.Attr.empty
      in
      let zone_classes = start_goal_classes pos in
      let cell_classes =
        [ "cell"
        ; (if alt then "cell--alt" else "")
        ; (if is_legal_dest pos then "cell--legal" else "")
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
    (* HUD: whose turn / skip button *)
    let hud =
      let turn_text =
        match st.decision with
        | Decision.Winner w -> sprintf "Winner: %s" (class_of_player w)
        | Decision.In_progress { whose_turn } ->
          sprintf "Turn: %s" (class_of_player whose_turn)
      in
      let skip_btn =
        Vdom.Node.button
          ~attrs:
            [ Vdom.Attr.class_ "btn"
            ; (if can_skip
               then Vdom.Attr.on_click (fun _ -> inject Skip_turn)
               else Vdom.Attr.empty)
            ; (if can_skip then Vdom.Attr.empty else Vdom.Attr.create "disabled" "")
            ]
          [ Vdom.Node.text "Skip turn" ]
      in
      Vdom.Node.div
        ~attrs:[ Vdom.Attr.class_ "hud" ]
        [ Vdom.Node.text turn_text
        ; Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "right" ]
            [ skip_btn
            ; (match model.selection with
               | None -> Vdom.Node.none
               | Some _ ->
                 Vdom.Node.button
                   ~attrs:
                     [ Vdom.Attr.class_ "btn"
                     ; Vdom.Attr.on_click (fun _ -> inject Clear_selection)
                     ]
                   [ Vdom.Node.text "Cancel" ])
            ]
        ]
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
  (* replace with whichever initial Game_state you want to show *)
  let initial_state = Game_state.create ~number_of_players:2 |> Or_error.ok_exn in
  Ui.component ~initial_state
;;

let () = Bonsai_web.Start.start app
