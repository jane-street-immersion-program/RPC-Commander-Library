open! Core
open Async
open Async_interactive

module Rpc_kind = struct
  type ('q, 'r, 'e) t =
    | Plain_rpc of ('q, 'r) Rpc.Rpc.t
    | Pipe_rpc of ('q, 'r, 'e) Rpc.Pipe_rpc.t
    | One_way of 'q Rpc.One_way.t
end

type ('q, 'r, 'e) t =
  { name : string option
  ; endpoint_command : 'q Command.Param.t
  ; endpoint_to_hp : 'q -> Host_and_port.t Or_error.t Deferred.t
  ; rpc : ('q, 'r, 'e) Rpc_kind.t
  ; sexp_from_r : 'r -> Sexp.t
  ; query_input_option : 'q Endpoint_spec.Query_input_option.t
  }

let create ?alias endpoint (rpc : ('q, 'r, 'e) Rpc_kind.t) sexp_from_r =
  { name =
      Some
        (Option.value
           alias
           ~default:
             (match rpc with
             | Pipe_rpc pipe -> Rpc.Pipe_rpc.name pipe
             | Plain_rpc plain -> Rpc.Rpc.name plain
             | One_way one_d -> Rpc.One_way.name one_d))
  ; endpoint_command =
      (match endpoint.Endpoint_spec.param with
      | Endpoint_spec.Query_input_option.Use_param param -> param
      | Endpoint_spec.Query_input_option.Use_generated q_of_sexp ->
        let%map_open.Command s_exp = anon ("sexp" %: string) in
        q_of_sexp (Sexp.of_string s_exp))
  ; endpoint_to_hp = endpoint.Endpoint_spec.to_host_and_port
  ; rpc
  ; sexp_from_r
  ; query_input_option = endpoint.Endpoint_spec.param
  }
;;

let create_pipe ?alias endpoint pipe_rpc sexp_from_r =
  create ?alias endpoint (Rpc_kind.Pipe_rpc pipe_rpc) sexp_from_r
;;

let create_plain ?alias endpoint plain_rpc sexp_from_r =
  create ?alias endpoint (Rpc_kind.Plain_rpc plain_rpc) sexp_from_r
;;

let create_one_way ?alias endpoint one_way sexp_from_r =
  create ?alias endpoint (Rpc_kind.One_way one_way) sexp_from_r
;;

let to_command_group' t =
  let name =
    match t.name with
    | Some alias -> alias
    | None ->
      (match t.rpc with
      | Pipe_rpc pipe -> Rpc.Pipe_rpc.name pipe
      | Plain_rpc plain -> Rpc.Rpc.name plain
      | One_way one_d -> Rpc.One_way.name one_d)
  in
  ( name
  , Command.async
      ~summary:name
      (match t.rpc with
      | Pipe_rpc pipe ->
        let%map.Command output = t.endpoint_command in
        let send_query query =
          let%bind h_and_p = t.endpoint_to_hp query >>| ok_exn in
          Rpc.Connection.with_client
            (Tcp.Where_to_connect.of_host_and_port h_and_p)
            (fun connection ->
              let%bind.Deferred pipe_reader, _ =
                Rpc.Pipe_rpc.dispatch_exn pipe connection query
              in
              Pipe.iter_without_pushback pipe_reader ~f:(fun response ->
                  Core.print_s [%message "Response received"];
                  let return_string =
                    Sexp.to_string_hum (t.sexp_from_r response)
                  in
                  Core.print_s [%message return_string]))
          >>| Result.ok_exn
        in
        fun () -> send_query output
      | Plain_rpc plain ->
        let%map.Command output = t.endpoint_command in
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
        fun () -> send_query output
      | One_way one_d ->
        let%map.Command output = t.endpoint_command in
        let send_query query =
          let%bind h_and_p = t.endpoint_to_hp query >>| ok_exn in
          Rpc.Connection.with_client
            (Tcp.Where_to_connect.of_host_and_port h_and_p)
            (fun connection ->
              return (Rpc.One_way.dispatch_exn one_d connection query))
          >>| Result.ok_exn
        in
        fun () -> send_query output) )
;;

let to_command_group t_list =
  Command.group
    ~summary:"Commander for RPCs"
    (List.map t_list ~f:(fun t_element -> to_command_group' t_element))
;;

module T_q_or_emptyline = struct
  type nonrec ('a, 'b, 'c) t =
    | Tuple of ('a, 'b, 'c) t * 'a
    | Empty_line of string
end

let rec parse_through_lines
    t_list
    (ref_list : (('a, 'b, 'c) t * 'a) list ref)
  =
  let%bind rpc_with_query =
    ask_dispatch_gen "()" ~f:(fun line ->
        let length = String.length line in
        if length <= 0
        then Ok (T_q_or_emptyline.Empty_line "")
        else (
          let first_space = String.index_from line 0 ' ' in
          match first_space with
          | None -> Error "invalid RPC name"
          | Some num ->
            let first_space = num in
            let first_word = String.sub line ~pos:0 ~len:first_space in
            let valid_t =
              List.find t_list ~f:(fun curr_t ->
                  String.equal
                    first_word
                    (match curr_t.name with
                    | Some alias -> alias
                    | None ->
                      (match curr_t.rpc with
                      | Pipe_rpc pipe -> Rpc.Pipe_rpc.name pipe
                      | Plain_rpc plain -> Rpc.Rpc.name plain
                      | One_way one_d -> Rpc.One_way.name one_d)))
            in
            let sexp_value =
              String.sub
                line
                ~pos:(first_space + 1)
                ~len:(length - first_space - 1)
            in
            (match valid_t with
            | None -> Error "No RPC with this name"
            | Some t ->
              let t_used = t in
              (try
                 match t_used.query_input_option with
                 | Endpoint_spec.Query_input_option.Use_param _ ->
                   Error
                     "You're trying to use an RPC that has your own PARAM \
                      and not the autogenerated sexp to query function"
                 | Endpoint_spec.Query_input_option.Use_generated q_of_sexp
                   ->
                   let query = q_of_sexp (Sexp.of_string sexp_value) in
                   Ok (T_q_or_emptyline.Tuple (t_used, query))
               with
              | _ -> Error "Bad sexp"))))
  in
  match rpc_with_query with
  | Tuple (rpc, q) ->
    parse_through_lines t_list (ref (!ref_list @ [ rpc, q ]))
  | Empty_line _ -> return ref_list
;;

let validate_hp t_q_list =
  let%map hp_list =
    Deferred.List.map !t_q_list ~f:(fun (t, q) ->
        let%map curr_hp = t.endpoint_to_hp q in
        curr_hp)
  in
  let hp_set =
    List.dedup_and_sort
      ~compare:[%compare: Host_and_port.t Or_error.t]
      hp_list
  in
  if List.length hp_set = 1 then true else false
;;

let read_until_dispatch (all_cmds : ('q, 'r, 'e) t list) =
  let%bind rpc_query_list = parse_through_lines all_cmds (ref []) in
  let%bind is_it_valid = validate_hp rpc_query_list in
  match is_it_valid with
  | false -> failwith "RPCs have different host_and_port"
  | true ->
    (match List.hd !rpc_query_list with
    | None -> failwith "You didn't pass in anything"
    | Some (t, q) ->
      let send_query query =
        match t.rpc with
        | Pipe_rpc _ -> failwith "implement this later"
        | One_way _ -> failwith "implement this later"
        | Plain_rpc _ ->
          let%bind h_and_p = t.endpoint_to_hp query >>| ok_exn in
          Rpc.Connection.with_client
            (Tcp.Where_to_connect.of_host_and_port h_and_p)
            (fun connection ->
              let dummy =
                Deferred.List.iter !rpc_query_list ~f:(fun (rpc, q) ->
                    let%map.Deferred response =
                      Rpc.Rpc.dispatch_exn
                        (match rpc.rpc with
                        | Pipe_rpc _ -> failwith "implement this later"
                        | One_way _ -> failwith "implement this later"
                        | Plain_rpc plain -> plain)
                        connection
                        q
                    in
                    Core.print_s [%message "Response received"];
                    let return_string =
                      Sexp.to_string_hum (t.sexp_from_r response)
                    in
                    Core.print_s [%message return_string])
              in
              dummy)
          >>| Result.ok_exn
      in
      send_query q)
;;

let rec loop_until_hard_close
    (f : ('q, 'r, 'e) t list -> unit Deferred.t)
    (input : ('q, 'r, 'e) t list)
  =
  let%bind output = f input in
  match output with _ -> loop_until_hard_close f input
;;

let multiple_rpcs t_list =
  Command.async
    ~summary:"multi-rpc in one connection"
    (let%map_open.Command () = return () in
     fun () -> loop_until_hard_close read_until_dispatch t_list)
;;
