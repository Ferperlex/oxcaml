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
