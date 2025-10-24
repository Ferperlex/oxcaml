open! Core
open Virtual_dom
open Chinese_checkers_logic_library
open Game
open! Bonsai.Let_syntax

module Layout = struct
  let size = 1.0
  let w = Float.sqrt 3.0 *. size
  let h = 2.0 *. size
  let half_w = w /. 2.0
  let half_h = h /. 2.0

  let xy_of_axial (q, r) =
    let qf = Float.of_int q
    and rf = Float.of_int r in
    let x = Float.sqrt 3.0 *. (qf +. (rf /. 2.0)) *. size in
    let y = 1.5 *. rf *. size in
    x, y
  ;;

  module Bounds = struct
    type t =
      { cx : float
      ; cy : float
      ; span : float
      }
  end

  let uniform_bounds (cells : (Cell_position.t * 'a) list) : Bounds.t =
    let xs, ys =
      List.map cells ~f:(fun (pos, _) -> xy_of_axial (pos.q_coordinate, pos.r_coordinate))
      |> List.unzip
    in
    let min_x = List.min_elt xs ~compare:Float.compare |> Option.value ~default:0. in
    let max_x = List.max_elt xs ~compare:Float.compare |> Option.value ~default:1. in
    let min_y = List.min_elt ys ~compare:Float.compare |> Option.value ~default:0. in
    let max_y = List.max_elt ys ~compare:Float.compare |> Option.value ~default:1. in
    let min_x = min_x -. half_w
    and max_x = max_x +. half_w in
    let min_y = min_y -. half_h
    and max_y = max_y +. half_h in
    let cx = (min_x +. max_x) /. 2.0 in
    let cy = (min_y +. max_y) /. 2.0 in
    let span = Float.max (max_x -. min_x) (max_y -. min_y) in
    { cx; cy; span }
  ;;

  let normalize ~bounds:(b : Bounds.t) ~(fit : float) (x, y) =
    let nx = ((x -. b.cx) /. b.span *. fit) +. 0.5 in
    let ny = ((y -. b.cy) /. b.span *. fit) +. 0.5 in
    nx *. 100.0, ny *. 100.0
  ;;

  let cell_size_pct ~bounds:(b : Bounds.t) ~(fit : float) ~(cell_scale : float) =
    let cw = w /. b.span *. fit *. cell_scale *. 100.0 in
    let ch = h /. b.span *. fit *. cell_scale *. 100.0 in
    cw, ch
  ;;
end

let pos_equal a b = Int.equal (Cell_position.compare a b) 0

let class_of_player = function
  | Player_kind.A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"
  | E -> "E"
  | F -> "F"
;;

let color_name_of_player = function
  | Player_kind.A -> "Blue"
  | B -> "Orange"
  | C -> "Green"
  | D -> "Black"
  | E -> "Purple"
  | F -> "Red"
;;

module Ui = struct
  module Screen = struct
    type t =
      | Landing
      | Playing of Game_state.t
    [@@deriving equal, sexp]
  end

  module Model = struct
    type t =
      { screen : Screen.t
      ; path : Cell_position.t list
      }
    [@@deriving equal, sexp]
  end

  let initial_model : Model.t = { screen = Landing; path = [] }

  module Action = struct
    type t =
      | Start_game of int
      | Select_start of Cell_position.t
      | Extend_path of Cell_position.t
      | Confirm_path
      | Cancel_path
  end

  let apply_action (m : Model.t) (a : Action.t) : Model.t =
    match a, m.screen with
    | Start_game n, _ ->
      (match Game_state.create ~number_of_players:n with
       | Ok st -> { screen = Playing st; path = [] }
       | Error _ -> m)
    | Cancel_path, Playing _ -> { m with path = [] }
    | Cancel_path, Landing -> m
    | Select_start pos, Playing st ->
      (match st.decision with
       | Decision.In_progress { whose_turn } ->
         (match Map.find st.board pos with
          | Some (Some who) when Player_kind.equal who whose_turn && List.is_empty m.path
            -> { m with path = [ pos ] }
          | _ -> m)
       | _ -> m)
    | Select_start _, Landing -> m
    | Extend_path dst, Playing st ->
      if List.is_empty m.path
      then m
      else (
        let nexts = Game_state.next_steps_from_path st ~path:m.path in
        if List.mem nexts dst ~equal:pos_equal
        then { m with path = m.path @ [ dst ] }
        else m)
    | Extend_path _, Landing -> m
    | Confirm_path, Playing st ->
      if List.length m.path < 2
      then m
      else (
        match Game_state.is_move_valid st m.path with
        | Ok () ->
          (match Game_state.make_move st m.path with
           | Ok st' -> { screen = Playing st'; path = [] }
           | Error _ -> { m with path = [] })
        | Error _ -> { m with path = [] })
    | Confirm_path, Landing -> m
  ;;

  let landing ~(inject : Action.t -> unit Ui_effect.t) : Vdom.Node.t =
    let cell lbl n extra_classes =
      Vdom.Node.div
        ~attrs:
          [ Vdom.Attr.classes ("landing__cell" :: extra_classes)
          ; Vdom.Attr.on_click (fun _ -> inject (Start_game n))
          ]
        [ Vdom.Node.div
            ~attrs:[ Vdom.Attr.class_ "landing__label" ]
            [ Vdom.Node.text lbl ]
        ]
    in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.class_ "landing" ]
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "landing__title" ]
          [ Vdom.Node.text "How many players?" ]
      ; Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ "landing__grid" ]
          [ cell "2" 2 [ "landing__cell--left"; "landing__cell--top" ]
          ; cell "3" 3 [ "landing__cell--top" ]
          ; cell "4" 4 [ "landing__cell--left" ]
          ; cell "6" 6 []
          ]
      ]
  ;;

  let playing_view
        (st : Game_state.t)
        (path : Cell_position.t list)
        ~(inject : Action.t -> unit Ui_effect.t)
    : Vdom.Node.t
    =
    let cells = Map.to_alist st.board in
    let bounds = Layout.uniform_bounds cells in
    let fit = 0.92 in
    let cell_scale = 0.74 in
    let cell_w, cell_h = Layout.cell_size_pct ~bounds ~fit ~cell_scale in
    let next_steps = Game_state.next_steps_from_path st ~path in
    let is_next pos = List.mem next_steps pos ~equal:pos_equal in
    let moving_owner, moving_head =
      match path with
      | start :: _ ->
        (match Map.find st.board start with
         | Some (Some who) -> Some who, List.last path
         | _ -> None, None)
      | [] -> None, None
    in
    let can_confirm =
      if List.length path >= 2
      then (
        match Game_state.is_move_valid st path with
        | Ok () -> true
        | Error _ -> false)
      else false
    in
    let turn_class =
      match st.decision with
      | Decision.In_progress { whose_turn } -> class_of_player whose_turn
      | Decision.Winner _ -> "none"
    in
    let render_cell ((pos : Cell_position.t), occ) =
      let x, y = Layout.xy_of_axial (pos.q_coordinate, pos.r_coordinate) in
      let left_pct, top_pct = Layout.normalize ~bounds ~fit (x, y) in
      let alt = (pos.q_coordinate + pos.r_coordinate) land 1 = 0 in
      let is_path_start =
        match path with
        | s :: _ -> pos_equal s pos
        | _ -> false
      in
      let base_piece =
        match occ with
        | None -> Vdom.Node.none
        | Some who ->
          let selectable =
            match st.decision, path with
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
    let hud =
      match path with
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
    let win_banner =
      match st.decision with
      | Decision.Winner who ->
        let who_name = color_name_of_player who in
        Vdom.Node.div
          ~attrs:[ Vdom.Attr.class_ ("banner banner--" ^ class_of_player who) ]
          [ Vdom.Node.text (who_name ^ " Wins!") ]
      | _ -> Vdom.Node.none
    in
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.classes [ "game"; "game--turn-" ^ turn_class ] ]
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.classes [ "board"; "board--turn-" ^ turn_class ] ]
          (List.map cells ~f:render_cell)
      ; hud
      ; win_banner
      ]
  ;;

  let view (model : Model.t) ~(inject : Action.t -> unit Ui_effect.t) : Vdom.Node.t =
    match model.screen with
    | Landing -> landing ~inject
    | Playing st -> playing_view st model.path ~inject
  ;;

  let component () =
    let%sub model, set_model = Bonsai.state (module Model) ~default_model:initial_model in
    let%arr model = model
    and set_model = set_model in
    let inject (a : Action.t) =
      let next = apply_action model a in
      set_model next
    in
    view model ~inject
  ;;
end

let app = Ui.component ()
let () = Bonsai_web.Start.start app
