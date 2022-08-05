open! Core
open Async

module Rpc_kind = struct
  type ('q, 'r, 'e) t =
    | Plain_rpc of ('q, 'r) Rpc.Rpc.t
    | Pipe_rpc of ('q, 'r, 'e) Rpc.Pipe_rpc.t
end

type ('q, 'r, 'e) t =
  { name : string option
  ; endpoint_command_list : 'q Command.Param.t list
  ; endpoint_to_hp : 'q -> Host_and_port.t Or_error.t Deferred.t
  ; rpc : ('q, 'r, 'e) Rpc_kind.t
  ; sexp_to_q : Sexp.t -> 'q
  ; sexp_from_r : 'r -> Sexp.t
  }

let create
    ?alias
    endpoint
    (rpc : ('q, 'r, 'e) Rpc_kind.t)
    sexp_to_q
    sexp_from_r
  =
  { name =
      Some
        (Option.value
           alias
           ~default:
             (match rpc with
             | Pipe_rpc pipe -> Rpc.Pipe_rpc.name pipe
             | Plain_rpc plain -> Rpc.Rpc.name plain))
  ; endpoint_command_list = [ endpoint.Endpoint_spec.param ]
  ; endpoint_to_hp = endpoint.Endpoint_spec.to_host_and_port
  ; rpc
  ; sexp_to_q
  ; sexp_from_r
  }
;;

let create_pipe ?alias endpoint pipe_rpc sexp_to_q sexp_from_r =
  create ?alias endpoint (Rpc_kind.Pipe_rpc pipe_rpc) sexp_to_q sexp_from_r
;;

let create_plain ?alias endpoint plain_rpc sexp_to_q sexp_from_r =
  create ?alias endpoint (Rpc_kind.Plain_rpc plain_rpc) sexp_to_q sexp_from_r
;;

let add_query_input_option t command_param =
  { name = t.name
  ; endpoint_command_list = command_param :: t.endpoint_command_list
  ; endpoint_to_hp = t.endpoint_to_hp
  ; rpc = t.rpc
  ; sexp_to_q = t.sexp_to_q
  ; sexp_from_r = t.sexp_from_r
  }
;;

let to_command_group t_list =
  Command.group
    ~summary:"Commander for RPCs"
    (List.map t_list ~f:(fun t_element ->
         let name =
           match t_element.name with
           | Some alias -> alias
           | None ->
             (match t_element.rpc with
             | Pipe_rpc pipe -> Rpc.Pipe_rpc.name pipe
             | Plain_rpc plain -> Rpc.Rpc.name plain)
         in
         ( name
         , Command.group
             ~summary:name
             (List.mapi
                ((let%map_open.Command s_exp = anon ("sexp" %: string) in
                  t_element.sexp_to_q (Sexp.of_string s_exp))
                :: t_element.endpoint_command_list)
                ~f:(fun index command_param ->
                  ( name ^ "-v" ^ string_of_int index
                  , Command.async
                      ~summary:
                        (match index with
                        | 0 -> "takes in sexp of query"
                        | _ ->
                          String.concat
                            ~sep:" "
                            (Command.Param.arg_names command_param))
                      (match t_element.rpc with
                      | Pipe_rpc pipe ->
                        let%map.Command output = command_param in
                        let send_query query =
                          let%bind h_and_p =
                            t_element.endpoint_to_hp query >>| ok_exn
                          in
                          Rpc.Connection.with_client
                            (Tcp.Where_to_connect.of_host_and_port h_and_p)
                            (fun connection ->
                              let%bind.Deferred pipe_reader, _ =
                                Rpc.Pipe_rpc.dispatch_exn
                                  pipe
                                  connection
                                  query
                              in
                              Pipe.iter_without_pushback
                                pipe_reader
                                ~f:(fun response ->
                                  Core.print_s [%message "Response received"];
                                  let return_string =
                                    Sexp.to_string_hum
                                      (t_element.sexp_from_r response)
                                  in
                                  Core.print_s [%message return_string]))
                          >>| Result.ok_exn
                        in
                        fun () -> send_query output
                      | Plain_rpc plain ->
                        let%map.Command output = command_param in
                        let send_query query =
                          let%bind h_and_p =
                            t_element.endpoint_to_hp query >>| ok_exn
                          in
                          Rpc.Connection.with_client
                            (Tcp.Where_to_connect.of_host_and_port h_and_p)
                            (fun connection ->
                              let%map.Deferred response =
                                Rpc.Rpc.dispatch_exn plain connection query
                              in
                              Core.print_s [%message "Response received"];
                              let return_string =
                                Sexp.to_string_hum
                                  (t_element.sexp_from_r response)
                              in
                              Core.print_s [%message return_string])
                          >>| Result.ok_exn
                        in
                        fun () -> send_query output) ))) )))
;;

let to_command_group' t =
  let name =
    match t.name with
    | Some alias -> alias
    | None ->
      (match t.rpc with
      | Pipe_rpc pipe -> Rpc.Pipe_rpc.name pipe
      | Plain_rpc plain -> Rpc.Rpc.name plain)
  in
  ( name
  , Command.group
      ~summary:name
      (List.mapi
         ((let%map_open.Command s_exp = anon ("sexp" %: string) in
           t.sexp_to_q (Sexp.of_string s_exp))
         :: t.endpoint_command_list)
         ~f:(fun index command_param ->
           ( name ^ "-v" ^ string_of_int index
           , Command.async
               ~summary:
                 (match index with
                 | 0 -> "takes in sexp of query"
                 | _ ->
                   String.concat
                     ~sep:" "
                     (Command.Param.arg_names command_param))
               (match t.rpc with
               | Pipe_rpc pipe ->
                 let%map.Command output = command_param in
                 let send_query query =
                   let%bind h_and_p = t.endpoint_to_hp query >>| ok_exn in
                   Rpc.Connection.with_client
                     (Tcp.Where_to_connect.of_host_and_port h_and_p)
                     (fun connection ->
                       let%bind.Deferred pipe_reader, _ =
                         Rpc.Pipe_rpc.dispatch_exn pipe connection query
                       in
                       Pipe.iter_without_pushback
                         pipe_reader
                         ~f:(fun response ->
                           Core.print_s [%message "Response received"];
                           let return_string =
                             Sexp.to_string_hum (t.sexp_from_r response)
                           in
                           Core.print_s [%message return_string]))
                   >>| Result.ok_exn
                 in
                 fun () -> send_query output
               | Plain_rpc plain ->
                 let%map.Command output = command_param in
                 let send_query query =
                   let%bind h_and_p = t.endpoint_to_hp query >>| ok_exn in
                   Rpc.Connection.with_client
                     (Tcp.Where_to_connect.of_host_and_port h_and_p)
                     (fun connection ->
                       let%map.Deferred response =
                         Rpc.Rpc.dispatch_exn plain connection query
                       in
                       Core.print_s [%message "Response received"];
                       let return_string =
                         Sexp.to_string_hum (t.sexp_from_r response)
                       in
                       Core.print_s [%message return_string])
                   >>| Result.ok_exn
                 in
                 fun () -> send_query output) ))) )
;;
