open Core
open Async

open Plnx
module Rest = Plnx_rest
module Ws = Plnx_ws_new

let default_cfg = Filename.concat (Option.value_exn (Sys.getenv "HOME")) ".virtu"
let find_auth cfg exchange =
  let cfg = Sexplib.Sexp.load_sexp_conv_exn cfg Cfg.t_of_sexp in
  let { Cfg.key ; secret ; _ } =
    List.Assoc.find_exn ~equal:String.equal cfg exchange in
  key, secret

let base_spec =
  let open Command.Spec in
  empty
  +> flag "-cfg" (optional_with_default default_cfg string) ~doc:"path Filepath of cfg (default: ~/.virtu)"
  +> flag "-testnet" no_arg ~doc:" Use testnet"
  +> flag "-md" no_arg ~doc:" Use multiplexing"
  +> flag "-rest" no_arg ~doc:" Tread stdin as input for REST commands"
  +> anon (sequence ("topic" %: string))

let src = Logs.Src.create "plnx.ws-test"
    ~doc:"Poloniex API - WS test application"

let plnx key secret topics =
  let process_user_cmd () =
    let process s =
      match String.split s ~on:' ' with
      | ["positions"] ->
        Rest.margin_positions ~key ~secret () >>= begin function
          | Error err ->
            Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
          | Ok resp ->
            Deferred.List.fold resp ~init:0 ~f:begin fun i (symbol, p) ->
              Logs_async.info begin fun m ->
                m "%s: %s" symbol
                  (Option.value_map p
                     ~default:"No position"
                     ~f:(fun p -> Rest.MarginPosition.sexp_of_t p |>
                                  Sexplib.Sexp.to_string))
              end >>| fun () ->
              succ i
            end >>= function
            | 0 -> Logs_async.info (fun m -> m "No positions")
            | _ -> Deferred.unit
        end
      | ["margin"] ->
        Rest.margin_account_summary ~key ~secret () >>= begin function
          | Error err -> Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
          | Ok resp ->
            Logs_async.info ~src (fun m -> m "%a" Sexp.pp (Rest.MarginAccountSummary.sexp_of_t resp))
        end
      | ["balance"; currency]  ->
        Rest.balances ~key ~secret () >>= begin function
          | Error err ->
            Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
          | Ok resp ->
            Logs_async.info ~src (fun m -> m "found %d balances" @@ List.length resp) >>= fun () ->
            match List.Assoc.find ~equal:String.equal resp currency with
            | None -> Deferred.unit
            | Some b ->
              Logs_async.info ~src begin fun m ->
                m "%s: %a" currency Sexp.pp (Rest.Balance.sexp_of_t b)
              end
        end
      | ["balances"]  ->
        Rest.positive_balances ~key ~secret () >>= begin function
          | Error err ->
            Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
          | Ok resp -> Deferred.List.iter resp ~f:begin fun (account, bs) ->
              Logs_async.info ~src begin fun m ->
                m "%a: %a"
                  Sexp.pp (Rest.Account.sexp_of_t account)
                  Sexp.pp Sexplib.Sexp.(List (List.map bs ~f:(fun (c, b) -> List [sexp_of_string c; sexp_of_float b])))
              end
            end
        end
      | ["oos"] ->
        Rest.open_orders ~key ~secret () >>= begin function
          | Ok resp -> begin
              Deferred.List.fold ~init:0  resp ~f:begin fun i (s, oos) ->
                if oos <> [] then begin
                  Logs_async.info ~src begin fun m ->
                    m "%s: %a" s Sexp.pp Sexplib.(Std.sexp_of_list Rest.OpenOrder.sexp_of_t oos)
                  end >>| fun () ->
                  succ i
                end
                else Deferred.return i
              end >>= function
              | 0 ->
                Logs_async.info ~src (fun m -> m "No open orders")
              | _ -> Deferred.unit
            end
          | Error err ->
            Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
        end
      | ["th"; symbol] ->
        Rest.trade_history ~symbol ~key ~secret () >>= begin function
          | Ok resp ->
            Deferred.List.iter resp ~f:begin fun (s, ths) ->
              Logs_async.info ~src begin fun m ->
                m "%s: %a" s Sexp.pp Sexplib.(Std.sexp_of_list Rest.TradeHistory.sexp_of_t ths)
              end
            end
          | Error err ->
            Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
        end
      | ["cancel"; id] ->
        let order_id = Int.of_string id in
        Rest.cancel_order ~key ~secret ~order_id () >>= begin function
          | Ok () ->
            Logs_async.info ~src (fun m -> m "canceled order %d OK" order_id)
          | Error err ->
            Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
        end
      | [side; symbol; price; qty] ->
        let side = Side.of_string side in
        let price = Float.of_string price in
        let qty = Float.of_string qty in
        if Plnx.margin_enabled symbol then
          Rest.submit_margin_order ~key ~secret ~symbol ~side ~price ~qty () >>= begin function
            | Ok resp -> Logs_async.info ~src (fun m -> m "%a" Sexp.pp (Rest.OrderResponse.sexp_of_t resp))
            | Error err -> Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
          end
        else
          Rest.submit_order ~key ~secret ~symbol ~side ~price ~qty () >>= begin function
            | Ok resp -> Logs_async.info ~src (fun m -> m "%a" Sexp.pp (Rest.OrderResponse.sexp_of_t resp))
            | Error err -> Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
          end
      | ["modify"; id; price] ->
        let order_id = Int.of_string id in
        let price = Float.of_string price in
        Rest.modify_order ~key ~secret ~price ~order_id () >>= begin function
          | Ok resp -> Logs_async.info ~src (fun m -> m "%a" Sexp.pp (Rest.OrderResponse.sexp_of_t resp))
          | Error err -> Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
        end
      (* | ["close"; symbol] -> *)
      (*   Rest.close_position ~key ~secret symbol >>| begin function *)
      (*   | Ok resp -> info "%s" (Rest.sexp_of_order_response resp |> Sexplib.Sexp.to_string) *)
      (*   | Error err -> error "%s" @@ Rest.Http_error.to_string err *)
      (*   end *)
      | h :: _ ->
        Logs_async.err (fun m -> m "Unknown command %s" h)
      | [] ->
        Logs_async.err ~src (fun m -> m "Empty command")
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
  let _restart, r = Ws.open_connection ~connected to_ws in
  let transfer_f msg =
    Format.asprintf "%a@." Sexplib.Sexp.pp_hum (Ws.Repr.sexp_of_t msg)
  in
  Deferred.all_unit [
    process_user_cmd ();
    Pipe.transfer r Writer.(pipe @@ Lazy.force stderr) ~f:transfer_f
  ]

let loglevel_of_int = function
  | 2 -> Logs.Info
  | 3 -> Debug
  | _ -> Error

let plnx =
  let run cfg _testnet _md _rest topics =
    let key, secret = find_auth cfg "PLNX" in
    don't_wait_for @@ plnx key secret topics;
    never_returns @@ Scheduler.go ()
  in
  Command.basic_spec ~summary:"Poloniex WS client" base_spec run

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

