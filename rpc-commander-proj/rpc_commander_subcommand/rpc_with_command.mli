open! Core
open Async

type ('q, 'r, 'e) t

val create_pipe
  :  ?alias:string
  -> 'q Endpoint_spec.t
  -> ('q, 'r, 'e) Rpc.Pipe_rpc.t
  -> (Sexp.t -> 'q)
  -> ('r -> Sexp.t)
  -> ('q, 'r, 'e) t

val create_plain
  :  ?alias:string
  -> 'q Endpoint_spec.t
  -> ('q, 'r) Rpc.Rpc.t
  -> (Sexp.t -> 'q)
  -> ('r -> Sexp.t)
  -> ('q, 'r, 'e) t

val add_query_input_option
  :  ('q, 'r, 'e) t
  -> 'q Command.Param.t
  -> ('q, 'r, 'e) t

val to_command_group : ('q, 'r, 'e) t list -> Command.t
val to_command_group' : ('q, 'r, 'e) t -> string * Command.t
