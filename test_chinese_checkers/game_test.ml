open! Core
open Chinese_checkers_logic_library
open Game

let valid q r =
  let s = -q - r in
  let aq, ar, as_ = Int.abs q, Int.abs r, Int.abs s in
  let m = Int.max aq (Int.max ar as_) in
  let smalls =
    (if aq <= 4 then 1 else 0) + (if ar <= 4 then 1 else 0) + if as_ <= 4 then 1 else 0
  in
  m <= 4 || (m <= 8 && smalls >= 2)
;;

let pretty_print_board ({ Game_state.board; decision; _ } : Game_state.t) =
  for r = -8 to 8 do
    let row_string =
      List.range ~start:`inclusive ~stop:`inclusive (-8) 8
      |> List.map ~f:(fun q ->
        match valid q r with
        | true ->
          let key = { Cell_position.q_coordinate = q; r_coordinate = r } in
          (match Map.find board key with
           | None -> " "
           | Some player ->
             (match player with
              | None -> "."
              | Some p -> Player_kind.sexp_of_t p |> Sexp.to_string))
        | false -> " ")
      |> String.concat ~sep:"  "
    in
    print_endline row_string
  done;
  print_s [%sexp (decision : Decision.t)]
;;

let random_walk ?(max_steps = 10_000) (initial_state : Game_state.t) ~random_seed =
  Core.Random.init random_seed;
  let rec loop (state : Game_state.t) (steps : int) : Game_state.t =
    match state.Game_state.decision with
    | Decision.Winner _ -> state
    | Decision.In_progress _ ->
      if steps >= max_steps
      then state
      else (
        match Game_state.has_any_legal_moves state with
        | false -> loop (Game_state.skip_turn state) (steps + 1)
        | true ->
          let moves = Game_state.all_legal_moves state in
          (match List.random_element moves with
           | None -> state (* shouldn't happen *)
           | Some mv ->
             (match Game_state.make_move state mv with
              | Ok next -> loop next (steps + 1)
              | Error _ -> loop state (steps + 1))))
    (* defensive, should be rare *)
  in
  loop initial_state 0
;;

let ai_players_sim ?(max_steps = 10_000) ?(depth = 6) (initial_state : Game_state.t)
  : Game_state.t
  =
  let rec loop (state : Game_state.t) (steps : int) : Game_state.t =
    match state.Game_state.decision with
    | Decision.Winner _ -> state
    | Decision.In_progress { whose_turn } ->
      if steps >= max_steps
      then state
      else (
        match Game_state.has_any_legal_moves state with
        | false -> loop (Game_state.skip_turn state) (steps + 1)
        | true ->
          let move_opt =
            match Ai.choose_move ~as_player:whose_turn ~depth state with
            | Some mv -> Some mv
            | None ->
              let moves = Game_state.all_legal_moves state in
              List.random_element moves
          in
          (match move_opt with
           | None ->
             (* Defensive fallback: if even that failed, skip *)
             loop (Game_state.skip_turn state) (steps + 1)
           | Some mv ->
             (match Game_state.make_move state mv with
              | Ok next_state -> loop next_state (steps + 1)
              | Error _ ->
                (* Very defensive: try another random move; if none works, skip. *)
                let others =
                  Game_state.all_legal_moves state
                  |> List.filter ~f:(fun m -> Move.compare m mv <> 0)
                in
                (match List.random_element others with
                 | Some mv2 ->
                   (match Game_state.make_move state mv2 with
                    | Ok next_state -> loop next_state (steps + 1)
                    | Error _ -> loop (Game_state.skip_turn state) (steps + 1))
                 | None -> loop (Game_state.skip_turn state) (steps + 1)))))
  in
  loop initial_state 0
;;

let make_move_and_print state mv =
  let new_state = Game_state.make_move state mv in
  match new_state with
  | Ok s -> pretty_print_board s
  | Error e -> print_s [%sexp (e : Error.t)]
;;

let initial_2p = Game_state.create ~number_of_players:2 |> Or_error.ok_exn
let initial_3p = Game_state.create ~number_of_players:3 |> Or_error.ok_exn
let initial_4p = Game_state.create ~number_of_players:4 |> Or_error.ok_exn
let initial_6p = Game_state.create ~number_of_players:6 |> Or_error.ok_exn

let%expect_test "initial 2 player game state" =
  pretty_print_board initial_2p;
  [%expect
    {|
                                        .
                                     .  .
                                  .  .  .
                               .  .  .  .
                .  .  .  .  .  .  .  .  .  A  A  A  A
                .  .  .  .  .  .  .  .  .  A  A  A
                .  .  .  .  .  .  .  .  .  A  A
                .  .  .  .  .  .  .  .  .  A
                .  .  .  .  .  .  .  .  .
             B  .  .  .  .  .  .  .  .  .
          B  B  .  .  .  .  .  .  .  .  .
       B  B  B  .  .  .  .  .  .  .  .  .
    B  B  B  B  .  .  .  .  .  .  .  .  .
                .  .  .  .
                .  .  .
                .  .
                .
    (In_progress (whose_turn A)) |}]
;;

let%expect_test "initial 3 player game state" =
  pretty_print_board initial_3p;
  [%expect
    {|
                                        .
                                     .  .
                                  .  .  .
                               .  .  .  .
                C  C  C  C  .  .  .  .  .  A  A  A  A
                C  C  C  .  .  .  .  .  .  A  A  A
                C  C  .  .  .  .  .  .  .  A  A
                C  .  .  .  .  .  .  .  .  A
                .  .  .  .  .  .  .  .  .
             .  .  .  .  .  .  .  .  .  .
          .  .  .  .  .  .  .  .  .  .  .
       .  .  .  .  .  .  .  .  .  .  .  .
    .  .  .  .  .  .  .  .  .  .  .  .  .
                B  B  B  B
                B  B  B
                B  B
                B
    (In_progress (whose_turn A)) |}]
;;

let%expect_test "initial 4 player game state" =
  pretty_print_board initial_4p;
  [%expect
    {|
                                        D
                                     D  D
                                  D  D  D
                               D  D  D  D
                .  .  .  .  .  .  .  .  .  A  A  A  A
                .  .  .  .  .  .  .  .  .  A  A  A
                .  .  .  .  .  .  .  .  .  A  A
                .  .  .  .  .  .  .  .  .  A
                .  .  .  .  .  .  .  .  .
             C  .  .  .  .  .  .  .  .  .
          C  C  .  .  .  .  .  .  .  .  .
       C  C  C  .  .  .  .  .  .  .  .  .
    C  C  C  C  .  .  .  .  .  .  .  .  .
                B  B  B  B
                B  B  B
                B  B
                B
    (In_progress (whose_turn A)) |}]
;;

let%expect_test "initial 6 player game state" =
  pretty_print_board initial_6p;
  [%expect
    {|
                                        F
                                     F  F
                                  F  F  F
                               F  F  F  F
                E  E  E  E  .  .  .  .  .  A  A  A  A
                E  E  E  .  .  .  .  .  .  A  A  A
                E  E  .  .  .  .  .  .  .  A  A
                E  .  .  .  .  .  .  .  .  A
                .  .  .  .  .  .  .  .  .
             D  .  .  .  .  .  .  .  .  B
          D  D  .  .  .  .  .  .  .  B  B
       D  D  D  .  .  .  .  .  .  B  B  B
    D  D  D  D  .  .  .  .  .  B  B  B  B
                C  C  C  C
                C  C  C
                C  C
                C
    (In_progress (whose_turn A)) |}]
;;

(* A truly random walk until end state will take very long to run as the end
state is so specific that it very unlikely to be reached *)
let random_walk_2p = random_walk initial_2p ~random_seed:10
let random_walk_3p = random_walk initial_3p ~random_seed:200
let random_walk_4p = random_walk initial_4p ~random_seed:3000
let random_walk_6p = random_walk initial_6p ~random_seed:40000

let%expect_test "random walk initial game state for differing player counts" =
  pretty_print_board random_walk_2p;
  [%expect
    {|
                                          .
                                       .  .
                                    .  .  A
                                 .  B  B  .
                  .  .  .  .  A  .  .  .  .  .  .  .  .
                  .  .  .  .  .  .  .  B  .  .  .  .
                  .  .  .  A  B  A  .  .  .  .  .
                  B  .  B  .  .  .  .  A  .  .
                  .  .  .  .  .  .  .  A  .
               .  .  .  .  B  .  B  .  .  B
            .  .  .  .  A  .  .  .  .  .  .
         A  .  .  A  .  .  .  .  .  B  .  .
      .  .  .  .  A  .  .  .  .  .  .  .  .
                  .  .  .  .
                  .  .  .
                  .  .
                  .
      (In_progress (whose_turn A))
    |}];
  pretty_print_board random_walk_3p;
  [%expect
    {|
                                          .
                                       .  .
                                    .  .  .
                                 .  .  .  .
                  .  .  C  .  .  .  .  .  .  A  .  .  .
                  C  .  .  .  C  A  B  .  A  .  .  .
                  .  .  .  .  B  B  C  .  .  A  B
                  C  B  C  .  B  .  .  .  .  .
                  .  B  .  .  .  .  A  .  .
               .  A  .  .  B  .  B  .  .  C
            .  A  A  .  .  .  .  .  .  .  .
         .  .  .  .  A  .  .  .  .  .  C  .
      .  .  A  .  C  .  .  C  .  .  .  .  .
                  .  .  .  .
                  .  .  .
                  B  .
                  .
      (In_progress (whose_turn B))
    |}];
  pretty_print_board random_walk_4p;
  [%expect
    {|
                                          .
                                       .  .
                                    .  .  .
                                 B  D  .  .
                  .  .  .  .  B  .  .  D  B  C  .  C  .
                  D  .  C  .  C  C  .  .  .  .  .  .
                  .  .  .  D  C  B  B  .  .  .  .
                  B  .  .  .  .  .  C  .  .  .
                  .  A  A  D  A  .  D  D  .
               .  .  .  .  .  .  C  .  .  .
            .  .  .  A  D  B  .  .  .  A  B
         .  .  C  .  .  B  .  .  A  C  D  .
      .  .  .  .  A  .  A  .  .  .  D  A  A
                  .  B  .  .
                  .  .  .
                  .  .
                  .
      (In_progress (whose_turn A))
    |}];
  pretty_print_board random_walk_6p;
  [%expect
    {|
                                          .
                                       F  C
                                    .  F  .
                                 .  .  .  .
                  .  B  .  B  D  C  F  F  C  D  D  .  .
                  .  .  .  D  E  C  D  B  E  .  .  .
                  .  E  E  C  A  .  .  .  .  A  .
                  .  B  B  .  .  A  F  .  .  .
                  B  .  .  .  B  .  .  B  F
               A  .  .  A  .  D  C  .  B  E
            .  A  .  F  C  D  .  E  .  .  E
         .  .  A  .  .  A  .  .  D  E  .  E
      .  A  A  .  .  B  D  E  D  .  .  .  .
                  .  .  C  F
                  F  C  .
                  .  C
                  F
      (In_progress (whose_turn E))
    |}]
;;

let%expect_test "get all legal moves from initial 2 player game state" =
  let all_moves = Game_state.all_legal_moves initial_2p in
  print_s [%message "All moves to begin 2 player game" (all_moves : Move.t list)];
  [%expect
    {|
    ("All moves to begin 2 player game"
     (all_moves
      ((((q_coordinate 5) (r_coordinate -4))
        ((q_coordinate 4) (r_coordinate -4)))
       (((q_coordinate 5) (r_coordinate -4))
        ((q_coordinate 4) (r_coordinate -3)))
       (((q_coordinate 5) (r_coordinate -3))
        ((q_coordinate 4) (r_coordinate -3)))
       (((q_coordinate 5) (r_coordinate -3))
        ((q_coordinate 4) (r_coordinate -2)))
       (((q_coordinate 5) (r_coordinate -2))
        ((q_coordinate 4) (r_coordinate -2)))
       (((q_coordinate 5) (r_coordinate -2))
        ((q_coordinate 4) (r_coordinate -1)))
       (((q_coordinate 5) (r_coordinate -1))
        ((q_coordinate 4) (r_coordinate -1)))
       (((q_coordinate 5) (r_coordinate -1)) ((q_coordinate 4) (r_coordinate 0)))
       (((q_coordinate 6) (r_coordinate -4))
        ((q_coordinate 4) (r_coordinate -4)))
       (((q_coordinate 6) (r_coordinate -4))
        ((q_coordinate 4) (r_coordinate -2)))
       (((q_coordinate 6) (r_coordinate -3))
        ((q_coordinate 4) (r_coordinate -3)))
       (((q_coordinate 6) (r_coordinate -3))
        ((q_coordinate 4) (r_coordinate -1)))
       (((q_coordinate 6) (r_coordinate -2))
        ((q_coordinate 4) (r_coordinate -2)))
       (((q_coordinate 6) (r_coordinate -2)) ((q_coordinate 4) (r_coordinate 0)))))) |}]
;;

let%expect_test "all moves from end of 6p random walk" =
  let all_moves = Game_state.all_legal_moves random_walk_6p in
  print_s [%message "All moves at end of 6 player random walk" (all_moves : Move.t list)];
  [%expect
    {|
      ("All moves at end of 6 player random walk"
       (all_moves
        ((((q_coordinate -3) (r_coordinate -2))
          ((q_coordinate -4) (r_coordinate -2)))
         (((q_coordinate -3) (r_coordinate -2))
          ((q_coordinate -4) (r_coordinate -1)))
         (((q_coordinate -3) (r_coordinate -2))
          ((q_coordinate -3) (r_coordinate -3)))
         (((q_coordinate -3) (r_coordinate -2))
          ((q_coordinate -3) (r_coordinate 0)))
         (((q_coordinate -3) (r_coordinate -2))
          ((q_coordinate -2) (r_coordinate -3)))
         (((q_coordinate -2) (r_coordinate -2))
          ((q_coordinate -4) (r_coordinate -2)))
         (((q_coordinate -2) (r_coordinate -2))
          ((q_coordinate -2) (r_coordinate -3)))
         (((q_coordinate -2) (r_coordinate -2))
          ((q_coordinate -2) (r_coordinate 0)))
         (((q_coordinate -1) (r_coordinate 4))
          ((q_coordinate -1) (r_coordinate 3)))
         (((q_coordinate -1) (r_coordinate 4)) ((q_coordinate 0) (r_coordinate 3)))
         (((q_coordinate -1) (r_coordinate 4)) ((q_coordinate 1) (r_coordinate 4)))
         (((q_coordinate -1) (r_coordinate 4)) ((q_coordinate 1) (r_coordinate 4))
          ((q_coordinate 3) (r_coordinate 2)))
         (((q_coordinate 0) (r_coordinate -3))
          ((q_coordinate -2) (r_coordinate -3)))
         (((q_coordinate 0) (r_coordinate -3))
          ((q_coordinate -2) (r_coordinate -3))
          ((q_coordinate -4) (r_coordinate -1)))
         (((q_coordinate 0) (r_coordinate -3))
          ((q_coordinate -2) (r_coordinate -3))
          ((q_coordinate -4) (r_coordinate -1))
          ((q_coordinate -4) (r_coordinate 1)))
         (((q_coordinate 0) (r_coordinate -3))
          ((q_coordinate -2) (r_coordinate -3))
          ((q_coordinate -4) (r_coordinate -1))
          ((q_coordinate -4) (r_coordinate 1)) ((q_coordinate -6) (r_coordinate 3))
          ((q_coordinate -4) (r_coordinate 3)))
         (((q_coordinate 0) (r_coordinate -3))
          ((q_coordinate 0) (r_coordinate -1)))
         (((q_coordinate 1) (r_coordinate 2)) ((q_coordinate 0) (r_coordinate 2)))
         (((q_coordinate 1) (r_coordinate 2)) ((q_coordinate 0) (r_coordinate 3)))
         (((q_coordinate 1) (r_coordinate 2)) ((q_coordinate 1) (r_coordinate 0)))
         (((q_coordinate 1) (r_coordinate 2)) ((q_coordinate 1) (r_coordinate 0))
          ((q_coordinate -1) (r_coordinate 0)))
         (((q_coordinate 1) (r_coordinate 2)) ((q_coordinate 1) (r_coordinate 0))
          ((q_coordinate 1) (r_coordinate -2)))
         (((q_coordinate 1) (r_coordinate 2)) ((q_coordinate 1) (r_coordinate 0))
          ((q_coordinate 3) (r_coordinate -2)))
         (((q_coordinate 1) (r_coordinate 2)) ((q_coordinate 1) (r_coordinate 4)))
         (((q_coordinate 1) (r_coordinate 2)) ((q_coordinate 1) (r_coordinate 4))
          ((q_coordinate 3) (r_coordinate 2)))
         (((q_coordinate 1) (r_coordinate 2)) ((q_coordinate 2) (r_coordinate 1)))
         (((q_coordinate 1) (r_coordinate 2)) ((q_coordinate 2) (r_coordinate 2)))
         (((q_coordinate 2) (r_coordinate 3)) ((q_coordinate 0) (r_coordinate 3)))
         (((q_coordinate 2) (r_coordinate 3)) ((q_coordinate 0) (r_coordinate 3))
          ((q_coordinate 2) (r_coordinate 1)))
         (((q_coordinate 2) (r_coordinate 3)) ((q_coordinate 0) (r_coordinate 3))
          ((q_coordinate 2) (r_coordinate 1)) ((q_coordinate 4) (r_coordinate -1)))
         (((q_coordinate 2) (r_coordinate 3)) ((q_coordinate 1) (r_coordinate 4)))
         (((q_coordinate 2) (r_coordinate 3)) ((q_coordinate 2) (r_coordinate 2)))
         (((q_coordinate 2) (r_coordinate 3)) ((q_coordinate 2) (r_coordinate 4)))
         (((q_coordinate 2) (r_coordinate 3)) ((q_coordinate 3) (r_coordinate 2)))
         (((q_coordinate 2) (r_coordinate 3)) ((q_coordinate 3) (r_coordinate 3)))
         (((q_coordinate 4) (r_coordinate -3))
          ((q_coordinate 3) (r_coordinate -2)))
         (((q_coordinate 4) (r_coordinate -3))
          ((q_coordinate 4) (r_coordinate -2)))
         (((q_coordinate 4) (r_coordinate 1)) ((q_coordinate 2) (r_coordinate 1)))
         (((q_coordinate 4) (r_coordinate 1)) ((q_coordinate 2) (r_coordinate 1))
          ((q_coordinate 0) (r_coordinate 3)))
         (((q_coordinate 4) (r_coordinate 1)) ((q_coordinate 2) (r_coordinate 1))
          ((q_coordinate 4) (r_coordinate -1)))
         (((q_coordinate 4) (r_coordinate 1)) ((q_coordinate 3) (r_coordinate 2)))
         (((q_coordinate 4) (r_coordinate 1)) ((q_coordinate 4) (r_coordinate -1)))
         (((q_coordinate 4) (r_coordinate 1)) ((q_coordinate 4) (r_coordinate -1))
          ((q_coordinate 2) (r_coordinate 1)))
         (((q_coordinate 4) (r_coordinate 1)) ((q_coordinate 4) (r_coordinate -1))
          ((q_coordinate 2) (r_coordinate 1)) ((q_coordinate 0) (r_coordinate 3)))
         (((q_coordinate 4) (r_coordinate 2)) ((q_coordinate 3) (r_coordinate 2)))
         (((q_coordinate 4) (r_coordinate 2)) ((q_coordinate 3) (r_coordinate 3)))
         (((q_coordinate 4) (r_coordinate 2)) ((q_coordinate 4) (r_coordinate 4)))
         (((q_coordinate 4) (r_coordinate 3)) ((q_coordinate 3) (r_coordinate 3)))
         (((q_coordinate 4) (r_coordinate 3)) ((q_coordinate 3) (r_coordinate 4)))
         (((q_coordinate 4) (r_coordinate 3)) ((q_coordinate 4) (r_coordinate 4))))))
       |}]
;;

let%expect_test "attempting move on opponent's piece" =
  make_move_and_print
    random_walk_6p
    [ { Cell_position.q_coordinate = 1; r_coordinate = 3 }
    ; { Cell_position.q_coordinate = 3; r_coordinate = 3 }
    ];
  [%expect
    {|
    ("It's not that player's turn" (owner D) (whose_turn E)) |}]
;;

let%expect_test "attempting move from outside the board" =
  make_move_and_print
    random_walk_6p
    [ { Cell_position.q_coordinate = 10; r_coordinate = 10 }
    ; { Cell_position.q_coordinate = 3; r_coordinate = 3 }
    ];
  [%expect
    {|
      ("Starting position not on board"
       (start ((q_coordinate 10) (r_coordinate 10)))) |}]
;;

let%expect_test "attempting move to opponent's goal" =
  make_move_and_print
    random_walk_6p
    [ { Cell_position.q_coordinate = 4; r_coordinate = -3 }
    ; { Cell_position.q_coordinate = 4; r_coordinate = -5 }
    ];
  [%expect
    {|
    ("Cannot land in an opponent's goal"
     (last ((q_coordinate 4) (r_coordinate -5)))) |}]
;;

let%expect_test "attempt move to occupied space" =
  make_move_and_print
    random_walk_6p
    [ { Cell_position.q_coordinate = 4; r_coordinate = -3 }
    ; { Cell_position.q_coordinate = 4; r_coordinate = -4 }
    ];
  [%expect
    {|
    ("Landing cell is occupied" (b ((q_coordinate 4) (r_coordinate -4)))) |}]
;;

let%expect_test "attempt invalid jump move" =
  make_move_and_print
    random_walk_6p
    [ { Cell_position.q_coordinate = 0; r_coordinate = -3 }
    ; { Cell_position.q_coordinate = 0; r_coordinate = 0 }
    ];
  [%expect
    {|
    ("First segment is not adjacent or a valid hop"
     (a ((q_coordinate 0) (r_coordinate -3)))
     (b ((q_coordinate 0) (r_coordinate 0)))) |}]
;;

let%expect_test "attempt move with only one cell_position" =
  make_move_and_print
    random_walk_6p
    [ { Cell_position.q_coordinate = 0; r_coordinate = -3 } ];
  [%expect
    {|
    "Move must have at least two positions" |}]
;;

let%expect_test "make valid multi-jump move" =
  make_move_and_print
    random_walk_2p
    [ { Cell_position.q_coordinate = 3; r_coordinate = 0 }
    ; { Cell_position.q_coordinate = 3; r_coordinate = -2 }
    ; { Cell_position.q_coordinate = 3; r_coordinate = -4 }
    ; { Cell_position.q_coordinate = 3; r_coordinate = -6 }
    ];
  [%expect
    {|
                                        .
                                     .  .
                                  .  A  A
                               .  B  B  .
                .  .  .  .  A  .  .  .  .  .  .  .  .
                .  .  .  .  .  .  .  B  .  .  .  .
                .  .  .  A  B  A  .  .  .  .  .
                B  .  B  .  .  .  .  A  .  .
                .  .  .  .  .  .  .  .  .
             .  .  .  .  B  .  B  .  .  B
          .  .  .  .  A  .  .  .  .  .  .
       A  .  .  A  .  .  .  .  .  B  .  .
    .  .  .  .  A  .  .  .  .  .  .  .  .
                .  .  .  .
                .  .  .
                .  .
                .
    (In_progress (whose_turn B)) |}]
;;

let%expect_test "make valid adjacent move" =
  make_move_and_print
    random_walk_2p
    [ { Cell_position.q_coordinate = 3; r_coordinate = 0 }
    ; { Cell_position.q_coordinate = 2; r_coordinate = 0 }
    ];
  [%expect
    {|
                                        .
                                     .  .
                                  .  .  A
                               .  B  B  .
                .  .  .  .  A  .  .  .  .  .  .  .  .
                .  .  .  .  .  .  .  B  .  .  .  .
                .  .  .  A  B  A  .  .  .  .  .
                B  .  B  .  .  .  .  A  .  .
                .  .  .  .  .  .  A  .  .
             .  .  .  .  B  .  B  .  .  B
          .  .  .  .  A  .  .  .  .  .  .
       A  .  .  A  .  .  .  .  .  B  .  .
    .  .  .  .  A  .  .  .  .  .  .  .  .
                .  .  .  .
                .  .  .
                .  .
                .
    (In_progress (whose_turn B)) |}]
;;

let%expect_test "AI players simulate 2 player game" =
  let final_state = ai_players_sim random_walk_2p ~max_steps:75 ~depth:2 in
  pretty_print_board final_state;
  [%expect
    {|
                                        .
                                     .  .
                                  .  .  .
                               .  .  .  .
                .  .  .  .  .  .  .  .  .  B  B  B  B
                .  .  .  .  .  .  .  .  .  B  B  B
                .  .  .  .  .  .  .  .  .  B  B
                .  .  .  .  .  .  .  .  .  B
                A  .  .  .  .  .  A  .  .
             .  .  .  .  .  .  .  .  .  .
          A  .  .  .  .  .  .  .  .  .  .
       A  A  A  .  .  .  .  .  .  .  .  .
    A  A  A  A  .  .  .  .  .  .  .  .  .
                .  .  .  .
                .  .  .
                .  .
                .
    (Winner B) |}]
;;

let%expect_test "AI players simulate 6 player game" =
  let final_state = ai_players_sim random_walk_6p ~max_steps:75 ~depth:2 in
  pretty_print_board final_state;
  [%expect
    {|
                                        C
                                     C  C
                                  C  C  C
                               .  .  F  .
                B  B  B  .  .  .  .  .  .  D  D  D  D
                B  B  .  C  .  .  .  B  .  D  D  D
                B  B  .  C  .  .  .  C  .  .  D
                B  .  .  C  B  .  .  .  .  D
                .  .  A  .  .  .  .  .  D
             .  .  .  .  .  .  .  .  .  E
          A  A  .  .  .  .  F  .  .  E  E
       A  A  A  .  .  .  .  .  .  E  E  E
    A  A  A  A  .  .  .  .  .  E  E  E  E
                F  F  .  .
                F  F  F
                F  F
                F
    (Winner E) |}]
;;

(* let%expect_test "AI players simulate 6 player game" =
  let final_state = ai_players_sim random_walk_3p ~max_steps:1 ~depth:4 in
  pretty_print_board final_state;
  [%expect
    {|
                                        C
                                     C  C
                                  C  C  C
                               .  .  F  .
                B  B  B  .  .  .  .  .  .  D  D  D  D
                B  B  .  C  .  .  .  B  .  D  D  D
                B  B  .  C  .  .  .  C  .  .  D
                B  .  .  C  B  .  .  .  .  D
                .  .  A  .  .  .  .  .  D
             .  .  .  .  .  .  .  .  .  E
          A  A  .  .  .  .  F  .  .  E  E
       A  A  A  .  .  .  .  .  .  E  E  E
    A  A  A  A  .  .  .  .  .  E  E  E  E
                F  F  .  .
                F  F  F
                F  F
                F
    (Winner E) |}]
;; *)
