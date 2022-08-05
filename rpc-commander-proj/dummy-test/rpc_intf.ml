open Core
open Async

let required_rpc =
  Rpc.Rpc.create
    ~name:"required_rpc"
    ~version:0
    ~bin_query:String.bin_t
    ~bin_response:String.bin_t
;;

let dummy_rpc =
  Rpc.Rpc.create
    ~name:"test_dummy"
    ~version:0
    ~bin_query:Unit.bin_t
    ~bin_response:Unit.bin_t
;;

let test_pipe_rpc =
  Rpc.Pipe_rpc.create
    ~name:"test_pipe"
    ~version:0
    ~bin_query:Unit.bin_t
    ~bin_response:Unit.bin_t
    ~bin_error:Nothing.bin_t
    ()
;;

let needs_required =
  Rpc.Rpc.create
    ~name:"needs_required"
    ~version:0
    ~bin_query:String.bin_t
    ~bin_response:String.bin_t
;;

let test_one_way =
  Rpc.One_way.create ~name:"test_one_way" ~version:0 ~bin_msg:String.bin_t
;;

module Playing_cards = struct
  module Value = struct
    type t =
      | Ace
      | King
      | Queen
      | Jack
      | Number of int
    [@@deriving bin_io, sexp]

    let to_string t = Sexp.to_string_hum (sexp_of_t t)

    let of_string s =
      try t_of_sexp (Sexp.of_string s) with _ -> Number (Int.of_string s)
    ;;

    let arg_type = Command.Arg_type.create of_string
  end

  module Suit = struct
    type t =
      | Clubs
      | Hearts
      | Spades
      | Diamonds
    [@@deriving bin_io, sexp]

    let to_string t = Sexp.to_string_hum (sexp_of_t t)
    let of_string s = t_of_sexp (Sexp.of_string s)
    let arg_type = Command.Arg_type.create of_string
  end

  type t =
    { value : Value.t
    ; suit : Suit.t
    }
  [@@deriving bin_io, sexp, fields]

  let to_string t = Sexp.to_string_hum (sexp_of_t t)
  let of_string s = t_of_sexp (Sexp.of_string s)
  let arg_type = Command.Arg_type.create of_string

  let random_card () =
    let rng_value = Random.int 13 in
    let rng_suit = Random.int 4 in
    let chosen_suit =
      match rng_suit with
      | 0 -> Suit.Clubs
      | 1 -> Suit.Hearts
      | 2 -> Suit.Spades
      | 3 -> Suit.Diamonds
      | _ -> raise (Failure "Number generated is outside of bounds")
    in
    let chosen_value =
      match rng_value with
      | 0 -> Value.Ace
      | 10 -> Value.Jack
      | 11 -> Value.Queen
      | 12 -> Value.King
      | _ -> Value.Number (rng_value + 1)
    in
    Fields.create ~value:chosen_value ~suit:chosen_suit
  ;;
end

module Response = struct
  type t = Playing_cards.t Or_error.t [@@deriving bin_io, sexp]
end

let card_rpc =
  Rpc.Rpc.create
    ~name:"playing_cards"
    ~version:0
    ~bin_query:Playing_cards.bin_t
    ~bin_response:Response.bin_t
;;
