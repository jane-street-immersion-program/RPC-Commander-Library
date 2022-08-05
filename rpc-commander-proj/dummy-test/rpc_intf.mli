open! Core
open! Async

(** Query for grabbing a unique ID *)
(* val get_unique_id : (unit, int) Rpc.Rpc.t *)

(** Query for setting the counter used to generate unique IDs *)
(* val set_id_counter : (int, unit) Rpc.Rpc.t *)

(** This is a deprecated query, no longer supported by the server *)
(* val set_id_counter_v0 : (int * int, unit) Rpc.Rpc.t *)

(** For getting a stream updating the counter values *)
val required_rpc : (string, string) Rpc.Rpc.t

val needs_required : (string, string) Rpc.Rpc.t
val dummy_rpc : (unit, unit) Rpc.Rpc.t
val test_pipe_rpc : (unit, unit, Nothing.t) Rpc.Pipe_rpc.t
val test_one_way : string Rpc.One_way.t

module Playing_cards : sig
  module Value : sig
    type t =
      | Ace
      | King
      | Queen
      | Jack
      | Number of int
    [@@deriving bin_io, sexp]

    val to_string : t -> string
    val of_string : string -> t
    val arg_type : t Command.Arg_type.t
  end

  module Suit : sig
    type t =
      | Clubs
      | Hearts
      | Spades
      | Diamonds
    [@@deriving bin_io, sexp]

    val to_string : t -> string
    val of_string : string -> t
    val arg_type : t Command.Arg_type.t
  end

  type t =
    { value : Value.t
    ; suit : Suit.t
    }
  [@@deriving bin_io, sexp, fields]

  val to_string : t -> string
  val of_string : string -> t
  val arg_type : t Command.Arg_type.t
  val random_card : unit -> t
end

module Response : sig
  type t = Playing_cards.t Or_error.t [@@deriving bin_io, sexp]
end

val card_rpc : (Playing_cards.t, Response.t) Rpc.Rpc.t
