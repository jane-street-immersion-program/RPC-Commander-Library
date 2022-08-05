open! Core
open Async

type ('q, 'r, 'e) t

val create_pipe
  :  ?alias:string
  -> 'q Endpoint_spec.t
  -> ('q, 'r, 'e) Rpc.Pipe_rpc.t
  -> ('r -> Sexp.t)
  -> ('q, 'r, 'e) t

val create_plain
  :  ?alias:string
  -> 'q Endpoint_spec.t
  -> ('q, 'r) Rpc.Rpc.t
  -> ('r -> Sexp.t)
  -> ('q, 'r, 'e) t

val create_one_way
  :  ?alias:string
  -> 'q Endpoint_spec.t
  -> 'q Rpc.One_way.t
  -> ('r -> Sexp.t)
  -> ('q, 'r, 'e) t

val to_command_group' : ('q, 'r, 'e) t -> string * Command.t
val to_command_group : ('q, 'r, 'e) t list -> Command.t
val multiple_rpcs : ('q, 'r, 'e) t list -> Command.t
