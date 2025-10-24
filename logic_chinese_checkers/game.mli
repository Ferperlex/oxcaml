open! Core

module Player_kind : sig
  type t =
    | A
    | B
    | C
    | D
    | E
    | F
  [@@deriving sexp, equal]
end

module Cell_position : sig
  type t =
    { q_coordinate : int
    ; r_coordinate : int
    }
  [@@deriving sexp, compare]

  include Comparable.S with type t := t
end

module Decision : sig
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Winner of Player_kind.t
  [@@deriving sexp]
end

module Move : sig
  type t = Cell_position.t list [@@deriving sexp, compare, equal]
end

module Game_state : sig
  type t =
    { board : Player_kind.t option Cell_position.Map.t
    ; number_of_players : int
    ; decision : Decision.t
    ; goals : (Player_kind.t * Cell_position.t list) list
    }
  [@@deriving sexp, equal]

  val start_triangle_for_player
    :  number_of_players:int
    -> Player_kind.t
    -> Cell_position.t list

  val create : number_of_players:int -> t Or_error.t
  val goals_for : t -> Player_kind.t -> Cell_position.t list
  val next_step_options : t -> path:Cell_position.t list -> Cell_position.t list
  val is_move_valid : t -> Move.t -> unit Or_error.t
  val players_in_game : int -> Player_kind.t list
  val all_legal_moves : t -> Move.t list
  val skip_turn : t -> t
  val has_any_legal_moves : t -> bool
  val make_move : t -> Move.t -> t Or_error.t
end

module Ai : sig
  val choose_move
    :  ?depth:int
    -> ?move_cap:int
    -> as_player:Player_kind.t
    -> Game_state.t
    -> Move.t option
end
