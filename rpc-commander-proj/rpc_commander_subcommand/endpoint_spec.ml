open Core
open Async

type 'a t =
  { param : 'a Command.Param.t
  ; to_host_and_port : 'a -> Host_and_port.t Or_error.t Deferred.t
  }
