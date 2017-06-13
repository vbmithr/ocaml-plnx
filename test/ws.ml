open Core
open Async
open Log.Global

open Plnx
module Rest = Plnx_rest
module Ws = Plnx_ws_new

let default_cfg = Filename.concat (Option.value_exn (Sys.getenv "HOME")) ".virtu"
let find_auth cfg exchange =
  let cfg = Sexplib.Sexp.load_sexp_conv_exn cfg Cfg.t_of_sexp in
  let { Cfg.key; secret } =
    List.Assoc.find_exn ~equal:String.equal cfg exchange in
  key, secret

let base_spec =
  let open Command.Spec in
  empty
  +> flag "-cfg" (optional_with_default default_cfg string) ~doc:"path Filepath of cfg (default: ~/.virtu)"
  +> flag "-loglevel" (optional int) ~doc:"1-3 loglevel"
  +> flag "-testnet" no_arg ~doc:" Use testnet"
  +> flag "-md" no_arg ~doc:" Use multiplexing"
  +> flag "-rest" no_arg ~doc:" Tread stdin as input for REST commands"
  +> anon (sequence ("topic" %: string))

let plnx key secret topics =
  let dft_log = Lazy.force log in
  let process_user_cmd () =
    let process s =
      match String.split s ~on:' ' with
      | ["positions"] ->
        Rest.margin_positions ~log:dft_log ~key ~secret () >>| begin function
        | Error err -> error "%s" @@ Rest.Http_error.to_string err
        | Ok resp ->
          let nb_nonempty = List.fold_left resp ~init:0 ~f:begin fun i (symbol, p) ->
              info "%s: %s" symbol
                (Option.value_map p
                   ~default:"No position"
                   ~f:(fun p -> Rest.MarginPosition.sexp_of_t p |>
                                Sexplib.Sexp.to_string));
              succ i
            end
          in
          if nb_nonempty = 0 then info "No positions"
        end
      | ["margin"] ->
        Rest.margin_account_summary ~key ~secret () >>| begin function
        | Error err -> error "%s" @@ Rest.Http_error.to_string err
        | Ok resp ->
          info "%s" (Rest.MarginAccountSummary.sexp_of_t resp |> Sexplib.Sexp.to_string)
        end
      | ["balance"; currency]  ->
        Rest.balances ~key ~secret () >>| begin function
        | Error err -> error "%s" (Rest.Http_error.to_string err)
        | Ok resp ->
          info "found %d balances" @@ List.length resp;
          match List.Assoc.find ~equal:String.equal resp currency with
          | Some b ->
            info "%s: %s" currency (Rest.Balance.sexp_of_t b |> Sexplib.Sexp.to_string)
        | None -> ()
        end
      | ["balances"]  ->
        Rest.positive_balances ~key ~secret () >>| begin function
        | Error err -> error "%s" @@ Rest.Http_error.to_string err
        | Ok resp -> List.iter resp ~f:begin fun (account, bs) ->
            info "%s: %s"
              (Rest.Account.sexp_of_t account |> Sexplib.Sexp.to_string)
              Sexplib.Sexp.(List (List.map bs ~f:(fun (c, b) -> List [sexp_of_string c; sexp_of_float b])) |> to_string)
          end
        end
      | ["oos"] ->
        Rest.open_orders ~key ~secret () >>| begin function
        | Ok resp ->
          let nb_nonempty = List.fold_left ~init:0  resp ~f:begin fun i (s, oos) ->
              if oos <> [] then begin
                info "%s: %s" s Sexplib.(Std.sexp_of_list Rest.OpenOrders.sexp_of_t oos |> Sexp.to_string);
                succ i
              end
              else i
            end
          in if nb_nonempty = 0 then info "No open orders"
        | Error err -> error "%s" @@ Rest.Http_error.to_string err
        end
      | ["th"; symbol] ->
        Rest.trade_history ~symbol ~key ~secret () >>| begin function
        | Ok resp ->
          List.iter resp ~f:begin fun (s, ths) ->
            info "%s: %s" s Sexplib.(Std.sexp_of_list Rest.TradeHistory.sexp_of_t ths |> Sexp.to_string)
          end
        | Error err -> error "%s" @@ Rest.Http_error.to_string err
        end
      | ["cancel"; id] ->
        let order_id = Int.of_string id in
        Rest.cancel_order ~key ~secret ~order_id () >>| begin function
        | Ok () -> info "canceled order %d OK" order_id
        | Error err -> error "%s" @@ Rest.Http_error.to_string err
        end
      | [side; symbol; price; qty] ->
        let side = match side with "buy" -> `Buy | "sell" -> `Sell | _ -> failwith "side" in
        let price = Float.of_string price in
        let qty = Float.of_string qty in
        if Plnx.margin_enabled symbol then
          Rest.margin_order ~key ~secret ~symbol ~side ~price ~qty () >>| begin function
          | Ok resp -> info "%s" (Rest.OrderResponse.sexp_of_t resp |> Sexplib.Sexp.to_string)
          | Error err -> error "%s" @@ Rest.Http_error.to_string err
          end
        else
          Rest.order ~key ~secret ~symbol ~side ~price ~qty () >>| begin function
          | Ok resp -> info "%s" (Rest.OrderResponse.sexp_of_t resp |> Sexplib.Sexp.to_string)
          | Error err -> error "%s" @@ Rest.Http_error.to_string err
          end
      | ["modify"; id; price] ->
        let order_id = Int.of_string id in
        let price = Float.of_string price in
        Rest.modify_order ~key ~secret ~price ~order_id () >>| begin function
        | Ok resp -> info "%s" (Rest.OrderResponse.sexp_of_t resp |> Sexplib.Sexp.to_string)
        | Error err -> error "%s" @@ Rest.Http_error.to_string err
        end
      (* | ["close"; symbol] -> *)
      (*   Rest.close_position ~key ~secret symbol >>| begin function *)
      (*   | Ok resp -> info "%s" (Rest.sexp_of_order_response resp |> Sexplib.Sexp.to_string) *)
      (*   | Error err -> error "%s" @@ Rest.Http_error.to_string err *)
      (*   end *)
      | h::t ->
        error "Unknown command %s" h; Deferred.unit
      | [] -> error "Empty command"; Deferred.unit
    in
    let rec loop () = Reader.(read_line @@ Lazy.force stdin) >>= function
      | `Eof -> Deferred.unit
      | `Ok line -> process line >>= loop
    in
    loop ()
  in
  let to_ws, to_ws_w = Pipe.create () in
  let connected = Condition.create () in
  don't_wait_for begin
    Condition.wait connected >>= fun () ->
    Deferred.List.iter topics ~f:(fun t -> Pipe.write to_ws_w (Ws.Repr.Subscribe t))
  end ;
  let _restart, r = Ws.open_connection ~connected ~log:(Lazy.force log) to_ws in
  let transfer_f msg =
      Format.asprintf "%a@." Sexplib.Sexp.pp_hum (Ws.Repr.sexp_of_t msg)
  in
  Deferred.all_unit [
    process_user_cmd ();
    Pipe.transfer r Writer.(pipe @@ Lazy.force stderr) ~f:transfer_f
  ]

let loglevel_of_int = function 2 -> `Info | 3 -> `Debug | _ -> `Error

let plnx =
  let run cfg loglevel _testnet _md _rest topics =
    Option.iter loglevel ~f:(Fn.compose set_level loglevel_of_int);
    let key, secret = find_auth cfg "PLNX" in
    don't_wait_for @@ plnx key secret topics;
    never_returns @@ Scheduler.go ()
  in
  Command.basic ~summary:"Poloniex WS client" base_spec run

(* let plnx_trades symbol = *)
(*   let r = Rest.all_trades_exn *)
(*       ~log:(Lazy.force log) symbol in *)
(*   let transfer_f t = DB.sexp_of_trade t |> Sexplib.Sexp.to_string |> fun s -> s ^ "\n" in *)
(*   Pipe.transfer r Writer.(pipe @@ Lazy.force stdout) ~f:transfer_f >>= fun () -> *)
(*   Shutdown.exit 0 *)

(* let plnx_trades = *)
(*   let run cfg loglevel _testnet _md _rest topics = *)
(*     Option.iter loglevel ~f:(Fn.compose set_level loglevel_of_int); *)
(*     begin match topics with *)
(*     | [] -> invalid_arg "topics" *)
(*     | currency :: _ -> don't_wait_for @@ plnx_trades currency; *)
(*     end; *)
(*     never_returns @@ Scheduler.go () *)
(*   in *)
(*   Command.basic ~summary:"Poloniex trades" base_spec run *)

let command =
  Command.group ~summary:"Exchanges WS client" [
      "plnx", plnx;
      (* "plnx-trades", plnx_trades; *)
    ]

let () = Command.run command

