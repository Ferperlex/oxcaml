open! Core

module Player_kind = struct
  type t =
    | A
    | B
    | C
    | D
    | E
    | F
  [@@deriving sexp]
end

module Cell_position = struct
  module T = struct
    type t =
      { q_coordinate : int
      ; r_coordinate : int
      }
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Decision = struct
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Winner of Player_kind.t
  [@@deriving sexp]
end

module Move = struct
  (* A move must visit at least two cells. We model this as a non-empty list
  with at least one "next" cell after the starting cell. *)
  type t = Cell_position.t list

  let create lst =
    match lst with
    | _first :: _second :: _rest ->
      (match List.find_a_dup lst ~compare:Poly.compare with
       | Some dup ->
         Or_error.error_s
           [%message "Move contains duplicate cell positions" (dup : Cell_position.t)]
       | None -> Ok lst)
    | _ -> Or_error.error_s [%message "Move must have at least two positions"]
  ;;
end

module Game_state = struct
  type t =
    { board : Player_kind.t option Cell_position.Map.t
    ; number_of_players : int
    ; decision : Decision.t
    }

  let create_empty_board () =
    let valid q r =
      let s = -q - r in
      let aq, ar, as_ = Int.abs q, Int.abs r, Int.abs s in
      let m = Int.max aq (Int.max ar as_) in
      let smalls =
        (if aq <= 4 then 1 else 0)
        + (if ar <= 4 then 1 else 0)
        + if as_ <= 4 then 1 else 0
      in
      m <= 4 || (m <= 8 && smalls >= 2)
    in
    let range = List.range ~start:`inclusive ~stop:`inclusive (-8) 8 in
    let coords =
      List.concat_map range ~f:(fun q ->
        List.filter_map range ~f:(fun r ->
          if valid q r
          then Some { Cell_position.q_coordinate = q; r_coordinate = r }
          else None))
    in
    List.fold coords ~init:Cell_position.Map.empty ~f:(fun acc key ->
      Map.set acc ~key ~data:None)
  ;;

  (* Helpers to construct triangles at each of the six star tips *)
  let invert_coordinate_signs cell_positions =
    List.map cell_positions ~f:(fun { Cell_position.q_coordinate; r_coordinate } ->
      { Cell_position.q_coordinate = -q_coordinate; r_coordinate = -r_coordinate })
  ;;

  let tri_pos_q () =
    List.concat_map [ 8; 7; 6; 5 ] ~f:(fun q ->
      let r_lo = Int.max (-4) (-q - 4)
      and r_hi = Int.min 4 (-q + 4) in
      List.range ~start:`inclusive ~stop:`inclusive r_lo r_hi
      |> List.map ~f:(fun r -> { Cell_position.q_coordinate = q; r_coordinate = r }))
  ;;

  let tri_neg_q () = invert_coordinate_signs (tri_pos_q ())

  let tri_pos_r () =
    List.concat_map [ 8; 7; 6; 5 ] ~f:(fun r ->
      let q_lo = Int.max (-4) (-r - 4)
      and q_hi = Int.min 4 (-r + 4) in
      List.range ~start:`inclusive ~stop:`inclusive q_lo q_hi
      |> List.map ~f:(fun q -> { Cell_position.q_coordinate = q; r_coordinate = r }))
  ;;

  let tri_neg_r () = invert_coordinate_signs (tri_pos_r ())

  let tri_pos_s () =
    List.concat_map [ 8; 7; 6; 5 ] ~f:(fun k ->
      let q_lo = Int.max (-4) (-k - 4)
      and q_hi = Int.min 4 (-k + 4) in
      List.range ~start:`inclusive ~stop:`inclusive q_lo q_hi
      |> List.map ~f:(fun q ->
        let r = -k - q in
        { Cell_position.q_coordinate = q; r_coordinate = r }))
  ;;

  let tri_neg_s () = invert_coordinate_signs (tri_pos_s ())

  let place_starting_positions ~board ~player ~cells =
    List.fold cells ~init:board ~f:(fun acc key -> Map.set acc ~key ~data:(Some player))
  ;;

  let populate_starting_positions ~number_of_players empty_board =
    let qpos = tri_pos_q ()
    and qneg = tri_neg_q ()
    and rpos = tri_pos_r ()
    and rneg = tri_neg_r ()
    and spos = tri_pos_s ()
    and sneg = tri_neg_s () in
    let assign pattern =
      List.fold pattern ~init:empty_board ~f:(fun board (player, cells) ->
        place_starting_positions ~board ~player ~cells)
    in
    match number_of_players with
    | 2 -> assign [ Player_kind.A, qpos; Player_kind.B, qneg ]
    | 3 -> assign [ Player_kind.A, qpos; Player_kind.B, rpos; Player_kind.C, spos ]
    | 4 ->
      assign
        [ Player_kind.A, qpos
        ; Player_kind.B, rpos
        ; Player_kind.C, qneg
        ; Player_kind.D, rneg
        ]
    | 6 ->
      assign
        [ Player_kind.A, qpos
        ; Player_kind.B, sneg
        ; Player_kind.C, rpos
        ; Player_kind.D, qneg
        ; Player_kind.E, spos
        ; Player_kind.F, rneg
        ]
    | _ ->
      failwith
        "invalid number of players has already been checked, this should never raise"
  ;;

  let create ~number_of_players =
    let number_of_players_ok =
      match number_of_players with
      (* 5 players not possible *)
      | 2 | 3 | 4 | 6 -> true
      | _ -> false
    in
    match number_of_players_ok with
    | false -> Or_error.error_s [%message "Invalid number of players"]
    | true ->
      let empty_board = create_empty_board () in
      Ok
        { board = populate_starting_positions ~number_of_players empty_board
        ; number_of_players
        ; decision = In_progress { whose_turn = A }
        }
  ;;
end
