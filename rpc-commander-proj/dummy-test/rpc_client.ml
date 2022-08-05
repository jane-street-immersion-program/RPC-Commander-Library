open Core
open Async
open Rpc_commander_lib

let send_both addr (arg : string) =
  Rpc.Connection.with_client
    (Tcp.Where_to_connect.of_host_and_port addr)
    (fun connection ->
      let%map.Deferred response =
        Rpc.Rpc.dispatch_exn Rpc_intf.required_rpc connection arg
      and response2 =
        Rpc.Rpc.dispatch_exn Rpc_intf.needs_required connection "works"
      in
      Core.print_s [%message "Response received"];
      Core.print_s [%message response];
      Core.print_s [%message response2])
  >>| Result.ok_exn
;;

let send_string addr (arg : string) =
  Rpc.Connection.with_client
    (Tcp.Where_to_connect.of_host_and_port addr)
    (fun connection ->
      let%map.Deferred response =
        Rpc.Rpc.dispatch_exn Rpc_intf.required_rpc connection arg
      in
      Core.print_s [%message "Response received"];
      Core.print_s [%message response])
  >>| Result.ok_exn
;;

let choose_card =
  let%map_open.Command playing_card_value =
    flag
      "-value"
      (required Rpc_intf.Playing_cards.Value.arg_type)
      ~doc:"Valid playing\n   card\n value: A,2,3,4...J,Q,K"
  and playing_card_suit =
    flag
      "-suit"
      (required Rpc_intf.Playing_cards.Suit.arg_type)
      ~doc:
        "Valid playing card\n   suit:\n Hearts, Spades, Diamonds, or Clubs"
  in
  Rpc_intf.Playing_cards.Fields.create
    ~value:playing_card_value
    ~suit:playing_card_suit
;;

let choose_string =
  let%map_open.Command chosen_string = anon ("string" %: string) in
  String.of_string chosen_string
;;

let string_cmd =
  Command.async
    ~summary:"string to string required"
    (let%map.Command string_chose = choose_string in
     fun () ->
       send_string
         { Host_and_port.host = "10.0.168.164"; port = 1444 }
         string_chose)
;;

let both_cmd =
  Command.async
    ~summary:"string to string required"
    (let%map.Command string_chose = choose_string in
     fun () ->
       send_both
         { Host_and_port.host = "10.0.168.164"; port = 1444 }
         string_chose)
;;

let to_hp (_ : _) =
  return (Ok { Host_and_port.host = "10.0.168.164"; port = 1444 })
;;

let () =
  let test_rpc_commander_dummy =
    Rpc_with_command.create_plain
      ?alias:(Some "dummy")
      { Endpoint_spec.param =
          Endpoint_spec.Query_input_option.use_generated unit_of_sexp
      ; to_host_and_port = to_hp
      }
      Rpc_intf.dummy_rpc
      sexp_of_unit
  in
  let test_rpc_commander_card =
    Rpc_with_command.create_plain
      ?alias:(Some "card")
      { Endpoint_spec.param =
          Endpoint_spec.Query_input_option.use_param choose_card
      ; to_host_and_port = to_hp
      }
      Rpc_intf.card_rpc
      (* Rpc_intf.Playing_cards.t_of_sexp *)
      Rpc_intf.Response.sexp_of_t
  in
  let test_pipe_rpc_commander =
    Rpc_with_command.create_pipe
      ?alias:(Some "pipe")
      { Endpoint_spec.param =
          Endpoint_spec.Query_input_option.use_generated unit_of_sexp
      ; to_host_and_port = to_hp
      }
      Rpc_intf.test_pipe_rpc
      sexp_of_unit
  in
  let test_one_way_commander =
    Rpc_with_command.create_one_way
      ?alias:(Some "one-way")
      { Endpoint_spec.param =
          Endpoint_spec.Query_input_option.use_generated string_of_sexp
      ; to_host_and_port = to_hp
      }
      Rpc_intf.test_one_way
      sexp_of_string
  in
  let required_commander =
    Rpc_with_command.create_plain
      ?alias:(Some "required")
      { Endpoint_spec.param =
          Endpoint_spec.Query_input_option.use_generated string_of_sexp
      ; to_host_and_port = to_hp
      }
      Rpc_intf.required_rpc
      sexp_of_string
  in
  let needs_required_commander =
    Rpc_with_command.create_plain
      ?alias:(Some "needs-required")
      { Endpoint_spec.param =
          Endpoint_spec.Query_input_option.use_generated string_of_sexp
      ; to_host_and_port = to_hp
      }
      Rpc_intf.needs_required
      sexp_of_string
  in
  let card_test, card_cmd =
    Rpc_with_command.to_command_group' test_rpc_commander_card
  in
  let dummy_test, dummy_cmd =
    Rpc_with_command.to_command_group' test_rpc_commander_dummy
  in
  let pipe_test, pipe_cmd =
    Rpc_with_command.to_command_group' test_pipe_rpc_commander
  in
  let one_way_test, one_way_cmd =
    Rpc_with_command.to_command_group' test_one_way_commander
  in
  let multiple_fun =
    Rpc_with_command.multiple_rpcs
      [ required_commander; needs_required_commander ]
  in
  Command_unix.run
    (Command.group
       ~summary:"Client for trivial Async-RPC server"
       [ dummy_test, dummy_cmd
       ; card_test, card_cmd
       ; pipe_test, pipe_cmd
       ; one_way_test, one_way_cmd
       ; "required", string_cmd
       ; "both", both_cmd
       ; "multi", multiple_fun
       ])
;;
