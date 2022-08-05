open Core
open Async

let return_unit _ _ = return ()

let return_card_string (_ : _) query =
  let ({ value; suit } : Rpc_intf.Playing_cards.t) = query in
  let return_string =
    "I have received your query: "
    ^ Rpc_intf.Playing_cards.Value.to_string value
    ^ " of "
    ^ Rpc_intf.Playing_cards.Suit.to_string suit
  in
  Core.print_s [%message return_string];
  let random_card = Rpc_intf.Playing_cards.random_card () in
  return (Ok random_card : Rpc_intf.Response.t)
;;

let one_way_impl (_ : _) query = Core.print_s [%message query]

let pipe_query (_ : _) _query =
  let reader, writer = Pipe.create () in
  Clock_ns.every (Time_ns.Span.of_ms 100.0) (fun () ->
      Core.print_s [%message "Signal Received"];
      Pipe.write_without_pushback_if_open writer ());
  return (Ok reader)
;;

let test_dependent (table : _) query =
  Core.print_s [%message query];
  (match Hashtbl.find table "required_rpc" with
  | None -> Hashtbl.add_exn table ~key:"required_rpc" ~data:"true"
  | Some _ -> ());
  return query
;;

let test_required_with_string (table : _) query =
  Core.print_s [%message query];
  match Hashtbl.find table "required_rpc" with
  | None -> return "you need to call the required_rpc beforehand"
  | Some _ -> return query
;;

let implementations =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:`Close_connection
    ~implementations:
      [ Rpc.Rpc.implement Rpc_intf.dummy_rpc return_unit
      ; Rpc.Rpc.implement Rpc_intf.card_rpc return_card_string
      ; Rpc.Pipe_rpc.implement Rpc_intf.test_pipe_rpc pipe_query
      ; Rpc.One_way.implement Rpc_intf.test_one_way one_way_impl
      ; Rpc.Rpc.implement Rpc_intf.required_rpc test_dependent
      ; Rpc.Rpc.implement Rpc_intf.needs_required test_required_with_string
      ]
;;

let serve port =
  let server =
    Tcp.Server.create
      (Tcp.Where_to_listen.of_port port)
      ~on_handler_error:`Ignore
      (fun (_ : Socket.Address.Inet.t) reader writer ->
        Rpc.Connection.server_with_close
          reader
          writer
          ~implementations
          ~connection_state:(fun (_ : Rpc.Connection.t) ->
            Hashtbl.create (module String))
          ~on_handshake_error:`Ignore)
  in
  ignore (server : (Socket.Address.Inet.t, int) Tcp.Server.t Deferred.t);
  never ()
;;

let command =
  Command.async
    ~summary:"start rpc server"
    (let%map_open.Command port =
       flag
         "-host-and-port"
         (required int)
         ~doc:"INT port that the server should listen on"
     in
     fun () -> serve port)
;;

let () = Command_unix.run command
