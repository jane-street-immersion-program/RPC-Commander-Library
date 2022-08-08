open! Core
open Async

module Query_input_option : sig
  type 'a t =
    | Use_param of 'a Command.Param.t
    | Use_generated of (Sexp.t -> 'a)

  val use_param : 'a Command.Param.t -> 'a t
  val use_generated : (Sexp.t -> 'a) -> 'a t
end

type 'a t =
  { param : 'a Query_input_option.t
  ; to_host_and_port : 'a -> Host_and_port.t Or_error.t Deferred.t
  }
