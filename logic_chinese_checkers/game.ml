open! Core

module Player_kind = struct
  type t =
    | A
    | B
    | C
    | D
    | E
    | F
  [@@deriving sexp, equal]
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
  [@@deriving sexp, equal]
end

module Move = struct
  type t = Cell_position.t list [@@deriving sexp, compare, equal]
end

module Game_state = struct
  type t =
    { board : Player_kind.t option Cell_position.Map.t
    ; number_of_players : int
    ; decision : Decision.t
    ; goals : (Player_kind.t * Cell_position.t list) list
    }
  [@@deriving sexp, equal]

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

  let invert_coordinate_signs cells =
    List.map cells ~f:(fun { Cell_position.q_coordinate; r_coordinate } ->
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
  let qpos = tri_pos_q ()
  let qneg = tri_neg_q ()
  let rpos = tri_pos_r ()
  let rneg = tri_neg_r ()
  let spos = tri_pos_s ()
  let sneg = tri_neg_s ()

  let players_in_game (n : int) : Player_kind.t list =
    match n with
    | 2 -> [ Player_kind.A; Player_kind.B ]
    | 3 -> [ Player_kind.A; Player_kind.B; Player_kind.C ]
    | 4 -> [ Player_kind.A; Player_kind.B; Player_kind.C; Player_kind.D ]
    | 6 ->
      [ Player_kind.A
      ; Player_kind.B
      ; Player_kind.C
      ; Player_kind.D
      ; Player_kind.E
      ; Player_kind.F
      ]
    | _ -> []
  ;;

  let start_triangle_for_player ~(number_of_players : int) (p : Player_kind.t)
    : Cell_position.t list
    =
    match number_of_players, p with
    | 2, Player_kind.A -> qpos
    | 2, Player_kind.B -> qneg
    | 3, Player_kind.A -> qpos
    | 3, Player_kind.B -> rpos
    | 3, Player_kind.C -> spos
    | 4, Player_kind.A -> qpos
    | 4, Player_kind.B -> rpos
    | 4, Player_kind.C -> qneg
    | 4, Player_kind.D -> rneg
    | 6, Player_kind.A -> qpos
    | 6, Player_kind.B -> sneg
    | 6, Player_kind.C -> rpos
    | 6, Player_kind.D -> qneg
    | 6, Player_kind.E -> spos
    | 6, Player_kind.F -> rneg
    | _, _ -> []
  ;;

  let goal_triangle_for_player ~(number_of_players : int) (p : Player_kind.t)
    : Cell_position.t list
    =
    match number_of_players, p with
    | 2, Player_kind.A -> qneg
    | 2, Player_kind.B -> qpos
    | 3, Player_kind.A -> qneg
    | 3, Player_kind.B -> rneg
    | 3, Player_kind.C -> sneg
    | 4, Player_kind.A -> qneg
    | 4, Player_kind.B -> rneg
    | 4, Player_kind.C -> qpos
    | 4, Player_kind.D -> rpos
    | 6, Player_kind.A -> qneg
    | 6, Player_kind.B -> spos
    | 6, Player_kind.C -> rneg
    | 6, Player_kind.D -> qpos
    | 6, Player_kind.E -> sneg
    | 6, Player_kind.F -> rpos
    | _, _ -> []
  ;;

  let place_starting_positions ~board ~player ~cells =
    List.fold cells ~init:board ~f:(fun acc key -> Map.set acc ~key ~data:(Some player))
  ;;

  let populate_starting_positions ~number_of_players empty_board =
    players_in_game number_of_players
    |> List.fold ~init:empty_board ~f:(fun b p ->
      place_starting_positions
        ~board:b
        ~player:p
        ~cells:(start_triangle_for_player ~number_of_players p))
  ;;

  let compute_goals ~(number_of_players : int)
    : (Player_kind.t * Cell_position.t list) list
    =
    players_in_game number_of_players
    |> List.map ~f:(fun p -> p, goal_triangle_for_player ~number_of_players p)
  ;;

  let goals_for (st : t) (p : Player_kind.t) : Cell_position.t list =
    List.Assoc.find_exn st.goals ~equal:Player_kind.equal p
  ;;

  let forbidden_for (st : t) (p : Player_kind.t) : Cell_position.t list =
    st.goals
    |> List.filter ~f:(fun (q, _) -> not (Player_kind.equal p q))
    |> List.concat_map ~f:snd
  ;;

  let create ~number_of_players =
    match number_of_players with
    | (2 | 3 | 4 | 6) as n ->
      let empty_board = create_empty_board () in
      let board = populate_starting_positions ~number_of_players:n empty_board in
      let goals = compute_goals ~number_of_players:n in
      Ok
        { board
        ; number_of_players = n
        ; decision = In_progress { whose_turn = Player_kind.A }
        ; goals
        }
    | _ -> Or_error.error_s [%message "Invalid number of players"]
  ;;

  let directions : (int * int) list = [ 1, 0; 0, 1; -1, 1; -1, 0; 0, -1; 1, -1 ]
  let double_directions = List.map directions ~f:(fun (dq, dr) -> 2 * dq, 2 * dr)

  let neighbor (pos : Cell_position.t) (dq, dr) : Cell_position.t =
    { Cell_position.q_coordinate = pos.q_coordinate + dq
    ; r_coordinate = pos.r_coordinate + dr
    }
  ;;

  let step_kind ~(from_ : Cell_position.t) ~(to_ : Cell_position.t) =
    let dq = to_.q_coordinate - from_.q_coordinate in
    let dr = to_.r_coordinate - from_.r_coordinate in
    match List.mem directions (dq, dr) ~equal:Poly.equal with
    | true -> `Adjacent
    | false ->
      (match List.mem double_directions (dq, dr) ~equal:Poly.equal with
       | true ->
         let mid =
           { Cell_position.q_coordinate = from_.q_coordinate + (dq / 2)
           ; r_coordinate = from_.r_coordinate + (dr / 2)
           }
         in
         `Hop_over mid
       | false -> `Invalid)
  ;;

  let landing_empty (board : Player_kind.t option Cell_position.Map.t) key =
    match Map.find board key with
    | Some None -> `Empty
    | Some (Some _) -> `Occupied
    | None -> `Off_board
  ;;

  let occupied (board : Player_kind.t option Cell_position.Map.t) key =
    match Map.find board key with
    | Some (Some _) -> `Occupied
    | Some None -> `Empty
    | None -> `Off_board
  ;;

  let adjacent_pairs (xs : 'a list) : ('a * 'a) list =
    let rec go acc = function
      | x :: (y :: _ as rest) -> go ((x, y) :: acc) rest
      | _ -> List.rev acc
    in
    go [] xs
  ;;

  let last_exn (xs : 'a list) : 'a =
    match List.last xs with
    | Some x -> x
    | None -> failwith "impossible: empty move after prior checks"
  ;;

  let pos_equal a b = Int.equal (Cell_position.compare a b) 0
  let pos_mem lst x = List.mem lst x ~equal:pos_equal

  let is_forbidden_landing (st : t) (p : Player_kind.t) (dest : Cell_position.t) : bool =
    match
      pos_mem (start_triangle_for_player ~number_of_players:st.number_of_players p) dest
    with
    | true -> false
    | false -> pos_mem (forbidden_for st p) dest
  ;;

  module Step_options = struct
    type t =
      { adjacents : Cell_position.t list
      ; hops : Cell_position.t list
      }
  end

  let next_steps_from_path (st : t) ~(path : Cell_position.t list) : Cell_position.t list =
    match st.decision with
    | Winner _ -> []
    | In_progress { whose_turn } ->
      (match path with
       | [] -> []
       | start :: _ ->
         (match Map.find st.board start with
          | Some (Some owner) when Player_kind.equal owner whose_turn ->
            let curr =
              match List.last path with
              | Some x -> x
              | None -> start
            in
            let last_seg_is_adjacent =
              match List.rev path with
              | _ :: prev :: _ ->
                (match step_kind ~from_:prev ~to_:curr with
                 | `Adjacent -> true
                 | _ -> false)
              | _ -> false
            in
            let hops_from (pos : Cell_position.t) : Cell_position.t list =
              directions
              |> List.filter_map ~f:(fun (dq, dr) ->
                let mid = neighbor pos (dq, dr) in
                let jump = neighbor pos (2 * dq, 2 * dr) in
                match occupied st.board mid, landing_empty st.board jump with
                | `Occupied, `Empty ->
                  if List.mem path jump ~equal:pos_equal then None else Some jump
                | _ -> None)
            in
            if List.length path = 1
            then (
              let adj =
                directions
                |> List.filter_map ~f:(fun dir ->
                  let dst = neighbor curr dir in
                  match landing_empty st.board dst with
                  | `Empty ->
                    if is_forbidden_landing st whose_turn dst then None else Some dst
                  | _ -> None)
              in
              let hops = hops_from curr in
              List.dedup_and_sort ~compare:Cell_position.compare (adj @ hops))
            else if last_seg_is_adjacent
            then []
            else hops_from curr |> List.dedup_and_sort ~compare:Cell_position.compare
          | _ -> []))
  ;;

  let next_step_options (st : t) ~(start : Cell_position.t) ~(path : Cell_position.t list)
    : Step_options.t
    =
    let curr =
      match List.last path with
      | Some x -> x
      | None -> start
    in
    let visited = path in
    let was_adjacent_first =
      match path with
      | a :: b :: _ ->
        (match step_kind ~from_:a ~to_:b with
         | `Adjacent -> true
         | _ -> false)
      | _ -> false
    in
    let first_step_taken = List.length path >= 2 in
    let adjacents =
      if first_step_taken
      then []
      else
        directions
        |> List.filter_map ~f:(fun dir ->
          let dst = neighbor curr dir in
          match Map.find st.board dst with
          | Some None -> Some dst
          | _ -> None)
    in
    let hops =
      if was_adjacent_first
      then []
      else
        directions
        |> List.filter_map ~f:(fun (dq, dr) ->
          let mid = neighbor curr (dq, dr) in
          let jump = neighbor curr (2 * dq, 2 * dr) in
          match Map.find st.board mid, Map.find st.board jump with
          | Some (Some _), Some None ->
            if
              List.mem visited jump ~equal:(fun a b ->
                Int.equal (Cell_position.compare a b) 0)
            then None
            else Some jump
          | _ -> None)
    in
    { Step_options.adjacents; hops }
  ;;

  let is_move_valid (st : t) (mv : Move.t) : unit Or_error.t =
    match mv with
    | [] | [ _ ] -> Or_error.error_s [%message "Move must have at least two positions"]
    | start :: _ ->
      (match Map.find st.board start with
       | None ->
         Or_error.error_s
           [%message "Starting position not on board" (start : Cell_position.t)]
       | Some None ->
         Or_error.error_s
           [%message "Starting position is empty" (start : Cell_position.t)]
       | Some (Some owner) ->
         (match st.decision with
          | Winner _ -> Or_error.error_s [%message "Game is already over"]
          | In_progress { whose_turn } ->
            (match Player_kind.equal owner whose_turn with
             | false ->
               Or_error.error_s
                 [%message
                   "It's not that player's turn"
                     (owner : Player_kind.t)
                     (whose_turn : Player_kind.t)]
             | true ->
               let segments = adjacent_pairs mv in
               (match segments with
                | [] -> Ok ()
                | (a, b) :: rest ->
                  (match step_kind ~from_:a ~to_:b with
                   | `Invalid ->
                     Or_error.error_s
                       [%message
                         "First segment is not adjacent or a valid hop"
                           (a : Cell_position.t)
                           (b : Cell_position.t)]
                   | `Adjacent ->
                     (match rest with
                      | _ :: _ ->
                        Or_error.error_s
                          [%message "Cannot combine step with hops in one move"]
                      | [] ->
                        (match landing_empty st.board b with
                         | `Empty ->
                           (match is_forbidden_landing st whose_turn b with
                            | true ->
                              Or_error.error_s
                                [%message
                                  "Cannot land in an opponent's goal"
                                    (b : Cell_position.t)]
                            | false -> Ok ())
                         | `Occupied ->
                           Or_error.error_s
                             [%message "Landing cell is occupied" (b : Cell_position.t)]
                         | `Off_board ->
                           Or_error.error_s
                             [%message
                               "Landing cell is off the board" (b : Cell_position.t)]))
                   | `Hop_over _ ->
                     let check_segment (from_, to_) =
                       match step_kind ~from_ ~to_ with
                       | `Adjacent ->
                         Or_error.error_s
                           [%message
                             "Cannot include an adjacent step in a hop sequence"
                               (from_ : Cell_position.t)
                               (to_ : Cell_position.t)]
                       | `Invalid ->
                         Or_error.error_s
                           [%message
                             "Segment is not a valid hop"
                               (from_ : Cell_position.t)
                               (to_ : Cell_position.t)]
                       | `Hop_over mid ->
                         (match occupied st.board mid with
                          | `Occupied ->
                            (match landing_empty st.board to_ with
                             | `Empty -> Ok ()
                             | `Occupied ->
                               Or_error.error_s
                                 [%message
                                   "Landing cell is occupied" (to_ : Cell_position.t)]
                             | `Off_board ->
                               Or_error.error_s
                                 [%message
                                   "Landing cell is off the board" (to_ : Cell_position.t)])
                          | `Empty ->
                            Or_error.error_s
                              [%message
                                "Must hop over an occupied piece" (mid : Cell_position.t)]
                          | `Off_board ->
                            Or_error.error_s
                              [%message
                                "Mid cell is off the board" (mid : Cell_position.t)])
                     in
                     Or_error.combine_errors_unit
                       (List.map ((a, b) :: rest) ~f:check_segment)
                     |> Or_error.bind ~f:(fun () ->
                       let last = last_exn mv in
                       match is_forbidden_landing st whose_turn last with
                       | true ->
                         Or_error.error_s
                           [%message
                             "Cannot land in an opponent's goal" (last : Cell_position.t)]
                       | false -> Ok ()))))))
  ;;

  let has_full_goal_for (st : t) (p : Player_kind.t) : bool =
    let goal_cells = goals_for st p in
    List.for_all goal_cells ~f:(fun pos ->
      match Map.find st.board pos with
      | Some (Some who) -> Player_kind.equal who p
      | _ -> false)
  ;;

  let has_any_legal_moves (st : t) : bool =
    match st.decision with
    | Decision.Winner _ -> false
    | Decision.In_progress { whose_turn } ->
      let any_step_from (start : Cell_position.t) : bool =
        let rec loop = function
          | [] -> false
          | (dq, dr) :: rest ->
            let dst = neighbor start (dq, dr) in
            (match landing_empty st.board dst with
             | `Empty ->
               (match is_forbidden_landing st whose_turn dst with
                | false -> true
                | true -> loop rest)
             | _ -> loop rest)
        in
        loop directions
      in
      let rec any_hop_from ~(curr : Cell_position.t) ~(visited : Cell_position.t list)
        : bool
        =
        let rec dirs = function
          | [] -> false
          | (dq, dr) :: rest ->
            let mid = neighbor curr (dq, dr) in
            let jump = neighbor curr (2 * dq, 2 * dr) in
            let can_hop =
              match Map.find st.board mid, Map.find st.board jump with
              | Some (Some _), Some None -> true
              | _ -> false
            in
            (match can_hop, List.mem visited jump ~equal:pos_equal with
             | true, false ->
               (match is_forbidden_landing st whose_turn jump with
                | false -> true
                | true ->
                  let visited' = jump :: visited in
                  (match any_hop_from ~curr:jump ~visited:visited' with
                   | true -> true
                   | false -> dirs rest))
             | _ -> dirs rest)
        in
        dirs directions
      in
      let my_positions =
        Map.to_alist st.board
        |> List.filter_map ~f:(fun (pos, occ) ->
          match occ with
          | Some who when Player_kind.equal who whose_turn -> Some pos
          | _ -> None)
      in
      let rec scan = function
        | [] -> false
        | pos :: rest ->
          (match any_step_from pos with
           | true -> true
           | false ->
             (match any_hop_from ~curr:pos ~visited:[ pos ] with
              | true -> true
              | false -> scan rest))
      in
      scan my_positions
  ;;

  let single_steps_from (st : t) (start : Cell_position.t) : Move.t list =
    directions
    |> List.filter_map ~f:(fun dir ->
      let dst = neighbor start dir in
      match Map.find st.board dst with
      | Some None -> Some [ start; dst ]
      | _ -> None)
  ;;

  let hop_sequences_from (st : t) (start : Cell_position.t) : Move.t list =
    let rec dfs (path : Cell_position.t list) acc =
      let curr =
        match List.last path with
        | Some x -> x
        | None -> failwith "empty hop path"
      in
      let hops =
        directions
        |> List.filter_map ~f:(fun (dq, dr) ->
          let mid = neighbor curr (dq, dr) in
          let jump = neighbor curr (2 * dq, 2 * dr) in
          match Map.find st.board mid, Map.find st.board jump with
          | Some (Some _), Some None when not (List.mem path jump ~equal:pos_equal) ->
            Some jump
          | _ -> None)
      in
      List.fold hops ~init:acc ~f:(fun acc jump ->
        let path' = path @ [ jump ] in
        let acc' = path' :: acc in
        dfs path' acc')
    in
    dfs [ start ] [] |> List.filter ~f:(fun p -> List.length p >= 2)
  ;;

  let all_legal_moves (st : t) : Move.t list =
    match st.decision with
    | Winner _ -> []
    | In_progress { whose_turn } ->
      let my_positions =
        Map.to_alist st.board
        |> List.filter_map ~f:(fun (pos, occ) ->
          match occ with
          | Some who when Player_kind.equal who whose_turn -> Some pos
          | _ -> None)
      in
      let candidates =
        List.concat_map my_positions ~f:(single_steps_from st)
        @ List.concat_map my_positions ~f:(hop_sequences_from st)
      in
      List.filter_map candidates ~f:(fun mv ->
        match is_move_valid st mv with
        | Ok () -> Some mv
        | Error _ -> None)
      |> List.dedup_and_sort ~compare:(List.compare Cell_position.compare)
  ;;

  let next_player ~(n : int) (p : Player_kind.t) : Player_kind.t =
    match n, p with
    | 2, Player_kind.A -> Player_kind.B
    | 2, Player_kind.B -> Player_kind.A
    | 3, Player_kind.A -> Player_kind.B
    | 3, Player_kind.B -> Player_kind.C
    | 3, Player_kind.C -> Player_kind.A
    | 4, Player_kind.A -> Player_kind.B
    | 4, Player_kind.B -> Player_kind.C
    | 4, Player_kind.C -> Player_kind.D
    | 4, Player_kind.D -> Player_kind.A
    | 6, Player_kind.A -> Player_kind.B
    | 6, Player_kind.B -> Player_kind.C
    | 6, Player_kind.C -> Player_kind.D
    | 6, Player_kind.D -> Player_kind.E
    | 6, Player_kind.E -> Player_kind.F
    | 6, Player_kind.F -> Player_kind.A
    | _, p -> p
  ;;

  let skip_turn (st : t) : t =
    match st.decision with
    | Decision.Winner _ -> st
    | Decision.In_progress { whose_turn } ->
      let nxt = next_player ~n:st.number_of_players whose_turn in
      { st with decision = Decision.In_progress { whose_turn = nxt } }
  ;;

  let make_move (st : t) (mv : Move.t) : t Or_error.t =
    match is_move_valid st mv with
    | Error _ as e -> e
    | Ok () ->
      (match mv, st.decision with
       | (start :: _ as path), In_progress { whose_turn } ->
         let last = last_exn path in
         let board' =
           Map.set
             (Map.set st.board ~key:start ~data:None)
             ~key:last
             ~data:(Some whose_turn)
         in
         let st' = { st with board = board' } in
         let decision' =
           match has_full_goal_for st' whose_turn with
           | true -> Decision.Winner whose_turn
           | false ->
             In_progress { whose_turn = next_player ~n:st.number_of_players whose_turn }
         in
         Ok { st' with decision = decision' }
       | _, _ -> Or_error.error_s [%message "Move must have at least two positions"])
  ;;
end

module Ai = struct
  let hex_distance (a : Cell_position.t) (b : Cell_position.t) : int =
    let dq = b.q_coordinate - a.q_coordinate in
    let dr = b.r_coordinate - a.r_coordinate in
    let ds = -(dq + dr) in
    (Int.abs dq + Int.abs dr + Int.abs ds) / 2
  ;;

  let pos_equal a b = Int.equal (Cell_position.compare a b) 0

  let remove_one ~equal x xs =
    let rec go acc = function
      | [] -> List.rev acc
      | y :: ys when equal x y -> List.rev_append acc ys
      | y :: ys -> go (y :: acc) ys
    in
    go [] xs
  ;;

  let ring_index (p : Cell_position.t) =
    let q = p.q_coordinate
    and r = p.r_coordinate in
    let s = -q - r in
    Int.max (Int.abs q) (Int.max (Int.abs r) (Int.abs s))
  ;;

  let pieces_of (st : Game_state.t) (p : Player_kind.t) : Cell_position.t list =
    Map.to_alist st.board
    |> List.filter_map ~f:(fun (pos, occ) ->
      match occ with
      | Some who when Player_kind.equal who p -> Some pos
      | _ -> None)
  ;;

  let greedy_goal_cost (st : Game_state.t) (p : Player_kind.t) : int =
    let goals =
      Game_state.goals_for st p
      |> List.sort ~compare:(fun a b -> Int.compare (ring_index b) (ring_index a))
    in
    let rec assign pieces goals acc =
      match goals with
      | [] -> acc
      | g :: gs ->
        let best_piece, best_d =
          match pieces with
          | [] -> g, 0
          | _ ->
            List.fold_left
              pieces
              ~init:(List.hd_exn pieces, Int.max_value)
              ~f:(fun (bp, bd) piece ->
                let d = hex_distance piece g in
                if d < bd then piece, d else bp, bd)
        in
        let pieces' = remove_one ~equal:pos_equal best_piece pieces in
        assign pieces' gs (acc + best_d)
    in
    assign (pieces_of st p) goals 0
  ;;

  let heuristic_value ~(for_player : Player_kind.t) (st : Game_state.t) : int =
    match st.decision with
    | Decision.Winner who ->
      if Player_kind.equal who for_player then Int.max_value else Int.min_value
    | Decision.In_progress _ ->
      let my_cost = greedy_goal_cost st for_player in
      let others =
        Game_state.players_in_game st.number_of_players
        |> List.filter ~f:(fun p -> not (Player_kind.equal p for_player))
      in
      let opp_best =
        match others with
        | [] -> 0
        | _ ->
          others
          |> List.map ~f:(fun p -> greedy_goal_cost st p)
          |> List.min_elt ~compare:Int.compare
          |> Option.value_exn
      in
      opp_best - my_cost
  ;;

  let children
        ?(move_cap : int option)
        (node : Game_state.t)
        ~(for_player : Player_kind.t)
    : (Move.t * Game_state.t) list
    =
    let moves = Game_state.all_legal_moves node in
    let kids =
      List.filter_map moves ~f:(fun mv ->
        match Game_state.make_move node mv with
        | Ok st' -> Some (mv, st')
        | Error _ -> None)
    in
    (* visit order aids pruning *)
    let compare_dir =
      match node.decision with
      | Decision.In_progress { whose_turn } ->
        if Player_kind.equal whose_turn for_player then Int.descending else Int.ascending
      | Decision.Winner _ -> Int.ascending
    in
    let sorted =
      List.sort kids ~compare:(fun (_m1, s1) (_m2, s2) ->
        Comparable.lift ~f:(heuristic_value ~for_player) compare_dir s1 s2)
    in
    match move_cap with
    | None -> sorted
    | Some k -> List.take sorted k
  ;;

  let rec alpha_beta
            ?(move_cap : int option)
            (node : Game_state.t)
            ~(for_player : Player_kind.t)
            (depth : int)
            (alpha : int)
            (beta : int)
    : int
    =
    match node.decision with
    | Decision.Winner _ -> heuristic_value ~for_player node
    | Decision.In_progress { whose_turn } ->
      if depth <= 0
      then heuristic_value ~for_player node
      else (
        let ch = children ?move_cap node ~for_player in
        if List.is_empty ch
        then (
          let skipped = Game_state.skip_turn node in
          alpha_beta ?move_cap skipped ~for_player (depth - 1) alpha beta)
        else if Player_kind.equal whose_turn for_player
        then (
          let best, _ =
            List.fold_until
              ch
              ~init:(Int.min_value, alpha)
              ~finish:(fun (v, _a) -> v, alpha)
              ~f:(fun (best, a) (_mv, child) ->
                let v =
                  Int.max best (alpha_beta ?move_cap child ~for_player (depth - 1) a beta)
                in
                let a' = Int.max a v in
                if v >= beta then Stop (v, a') else Continue (v, a'))
          in
          best)
        else (
          let best, _ =
            List.fold_until
              ch
              ~init:(Int.max_value, beta)
              ~finish:(fun (v, _b) -> v, beta)
              ~f:(fun (best, b) (_mv, child) ->
                let v =
                  Int.min
                    best
                    (alpha_beta ?move_cap child ~for_player (depth - 1) alpha b)
                in
                let b' = Int.min b v in
                if v <= alpha then Stop (v, b') else Continue (v, b'))
          in
          best))
  ;;

  let choose_move_with_depth
        ?(move_cap = 30)
        ~(as_player : Player_kind.t)
        ~(depth : int)
        (st : Game_state.t)
    : Move.t option
    =
    match st.decision with
    | Decision.Winner _ -> None
    | Decision.In_progress { whose_turn } ->
      let kids = children ~move_cap st ~for_player:as_player in
      if List.is_empty kids
      then None
      else (
        let scored =
          List.map kids ~f:(fun (mv, child) ->
            let v =
              alpha_beta
                ~move_cap
                child
                ~for_player:as_player
                (depth - 1)
                Int.min_value
                Int.max_value
            in
            mv, v)
        in
        let pick_max =
          List.max_elt scored ~compare:(fun (_m1, v1) (_m2, v2) -> Int.compare v1 v2)
        and pick_min =
          List.min_elt scored ~compare:(fun (_m1, v1) (_m2, v2) -> Int.compare v1 v2)
        in
        if Player_kind.equal whose_turn as_player
        then Option.map pick_max ~f:fst
        else Option.map pick_min ~f:fst)
  ;;

  let choose_move ?depth ?move_cap ~(as_player : Player_kind.t) (st : Game_state.t)
    : Move.t option
    =
    let depth = Option.value depth ~default:3 in
    let move_cap = Option.value move_cap ~default:30 in
    choose_move_with_depth ~move_cap ~as_player ~depth st
  ;;
end
