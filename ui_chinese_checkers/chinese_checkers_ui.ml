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
(* let last_exn xs = Option.value_exn (List.last xs) *)

let class_of_player = function
  | Player_kind.A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"
  | E -> "E"
  | F -> "F"
;;

let class_attrs (classes : string list) =
  Vdom.Attr.many (List.map classes ~f:Vdom.Attr.class_)
;;

(* auto-skip helper: repeatedly skip while the current player has no legal moves *)
let rec advance_skips (st : Game_state.t) : Game_state.t =
  match st.decision with
  | Decision.Winner _ -> st
  | Decision.In_progress _ ->
    if Game_state.has_any_legal_moves st
    then st
    else advance_skips (Game_state.skip_turn st)
;;

(* ---------- Bonsai UI ---------- *)
module Ui = struct
  type model =
    { game_state : Game_state.t
    ; path : Cell_position.t list option (* current composed path: [start; ...] *)
    ; last_move : Move.t option
    }
  [@@deriving equal, sexp]

  let initial_model game_state =
    (* Ensure play starts on someone who actually has a move *)
    { game_state = advance_skips game_state; path = None; last_move = None }
  ;;

  type action =
    | Select_start of Cell_position.t
    | Extend_to of Cell_position.t
    | Clear_path
    | Confirm_path

  let apply_action (model : model) (action : action) : model =
    match action with
    | Clear_path -> { model with path = None }
    | Select_start pos ->
      (* only selectable if it's your piece and game in progress *)
      (match model.game_state.decision with
       | Decision.Winner _ -> model
       | Decision.In_progress { whose_turn } ->
         (match Map.find model.game_state.board pos with
          | Some (Some who) when Player_kind.equal who whose_turn ->
            { model with path = Some [ pos ] }
          | _ -> model))
    | Extend_to dst ->
      (match model.path with
       | None -> model
       | Some p ->
         let nexts = Game_state.next_step_options model.game_state ~path:p in
         if List.mem nexts dst ~equal:pos_equal
         then { model with path = Some (p @ [ dst ]) }
         else model)
    | Confirm_path ->
      (match model.path with
       | Some p when List.length p >= 2 ->
         (match Game_state.make_move model.game_state p with
          | Ok st' ->
            let st'' = advance_skips st' in
            { game_state = st''; path = None; last_move = Some p }
          | Error _ -> model)
       | _ -> model)
  ;;

  let view (model : model) ~(inject : action -> unit Ui_effect.t) : Vdom.Node.t =
    let st = model.game_state in
    let cells = Map.to_alist st.board in
    let bbox = Layout.bbox_of_cells cells in
    let cell_w, cell_h = Layout.cell_size_pct ~bbox in
    (* current legal next landings for the composed path *)
    let legal_nexts =
      match model.path with
      | None -> []
      | Some p -> Game_state.next_step_options st ~path:p
    in
    let is_legal_next pos = List.mem legal_nexts pos ~equal:pos_equal in
    (* tint start/goal zones (every player's) *)
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
    let path_cells = Option.value model.path ~default:[] in
    let render_cell ((pos : Cell_position.t), occ) =
      let x, y = Layout.xy_of_axial (pos.q_coordinate, pos.r_coordinate) in
      let left_pct, top_pct = Layout.normalize ~bbox (x, y) in
      let alt = (pos.q_coordinate + pos.r_coordinate) land 1 = 0 in
      (* Click behavior:
         - If no path: clicking your own piece selects it.
         - If path exists: clicking a highlighted next cell extends the path.
      *)
      let on_click =
        match model.path, st.decision, occ with
        | None, Decision.In_progress { whose_turn }, Some who
          when Player_kind.equal who whose_turn ->
          Vdom.Attr.on_click (fun _ -> inject (Select_start pos))
        | Some _, _, _ when is_legal_next pos ->
          Vdom.Attr.on_click (fun _ -> inject (Extend_to pos))
        | _ -> Vdom.Attr.empty
      in
      let zone_classes = start_goal_classes pos in
      let in_path = List.mem path_cells pos ~equal:pos_equal in
      let cell_classes =
        [ "cell" ]
        @ (if alt then [ "cell--alt" ] else [])
        @ (if is_legal_next pos then [ "cell--legal" ] else [])
        @ (if in_path then [ "cell--path" ] else [])
        @ zone_classes
      in
      let piece =
        match occ with
        | None -> Vdom.Node.none
        | Some who ->
          let selected =
            match model.path with
            | Some (s :: _) when pos_equal s pos -> [ "piece--selected" ]
            | _ -> []
          in
          Vdom.Node.div
            ~attrs:
              [ class_attrs ([ "piece"; "piece--" ^ class_of_player who ] @ selected) ]
            []
      in
      let style =
        Css_gen.(
          left (`Percent (Percent.of_percentage left_pct))
          @> top (`Percent (Percent.of_percentage top_pct))
          @> width (`Percent (Percent.of_percentage cell_w))
          @> height (`Percent (Percent.of_percentage cell_h)))
      in
      Vdom.Node.div
        ~attrs:[ class_attrs cell_classes; Vdom.Attr.style style; on_click ]
        [ piece ]
    in
    (* Toolbar: only Confirm/Cancel when composing a path *)
    let toolbar =
      match model.path with
      | None -> Vdom.Node.none
      | Some p ->
        let can_confirm = List.length p >= 2 in
        Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "toolbar" ]
          [ Vdom.Node.div
              ~attrs:[ Vdom.Attr.class_ "toolbar__right" ]
              [ Vdom.Node.button
                  ~attrs:
                    [ Vdom.Attr.class_ "btn"
                    ; Vdom.Attr.on_click (fun _ -> inject Clear_path)
                    ]
                  [ Vdom.Node.text "Cancel" ]
              ; Vdom.Node.button
                  ~attrs:
                    ([ Vdom.Attr.class_ "btn"; Vdom.Attr.class_ "btn--primary" ]
                     @
                     if can_confirm
                     then [ Vdom.Attr.on_click (fun _ -> inject Confirm_path) ]
                     else [ Vdom.Attr.create "disabled" "" ])
                  [ Vdom.Node.text "Confirm move" ]
              ]
          ]
    in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "game" ]
      [ toolbar
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

(* Entry point *)
let app =
  let initial_state = Game_state.create ~number_of_players:2 |> Or_error.ok_exn in
  Ui.component ~initial_state
;;

let () = Bonsai_web.Start.start app
