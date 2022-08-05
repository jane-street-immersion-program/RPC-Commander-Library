open Core
open Async

module Query_input_option = struct
  type 'a t =
    | Use_param of 'a Command.Param.t
    | Use_generated of (Sexp.t -> 'a)

  let use_param param = Use_param param
  let use_generated gen = Use_generated gen
end

type 'a t =
  { param : 'a Query_input_option.t
  ; to_host_and_port : 'a -> Host_and_port.t Or_error.t Deferred.t
  }
