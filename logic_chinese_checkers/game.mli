open! Core

module Player_kind : sig
  type t =
    | A
    | B
    | C
    | D
    | E
    | F
  [@@deriving sexp]
end

module Cell_position : sig
  (* CR: write comment explaining coordinate system *)
  type t =
    { q_coordinate : int
    ; r_coordinate : int
    }

  include Comparable.S with type t := t
end

module Decision : sig
  type t =
    | In_progress of { whose_turn : Player_kind.t }
    | Winner of Player_kind.t
  [@@deriving sexp]
end

module Move : sig
  type t

  val create : Cell_position.t list -> t Or_error.t
end

module Game_state : sig
  type t =
    { board : Player_kind.t option Cell_position.Map.t
    ; number_of_players : int
    ; decision : Decision.t
    }

  val tri_pos_q : unit -> Cell_position.t list
  val tri_neg_q : unit -> Cell_position.t list
  val tri_pos_r : unit -> Cell_position.t list
  val tri_neg_r : unit -> Cell_position.t list
  val tri_pos_s : unit -> Cell_position.t list
  val tri_neg_s : unit -> Cell_position.t list
  val create : number_of_players:int -> t Or_error.t
end
