open Core
open Async

open Plnx
open Plnx_ws
module Rest = Plnx_rest

let src = Logs.Src.create "plnx.ws-test"
    ~doc:"Poloniex API - WS test application"

let process_user_cmd w =
  let process s =
    match String.split s ~on:' ' with
    | "subscribe" :: topics ->
      Deferred.List.iter topics ~f:begin fun t ->
        Pipe.write w (tradesQuotes (Pair.of_string_exn t))
      end
    | "unsubscribe" :: chanIDs ->
      Deferred.List.iter chanIDs ~f:begin fun id ->
        Pipe.write w (unsubscribe (Int.of_string id))
      end
    (* | ["positions"] ->
     *   Rest.margin_positions ~key ~secret () >>= begin function
     *     | Error err ->
     *       Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
     *     | Ok resp ->
     *       Deferred.List.fold resp ~init:0 ~f:begin fun i (symbol, p) ->
     *         Logs_async.info begin fun m ->
     *           m "%s: %s" symbol
     *             (Option.value_map p
     *                ~default:"No position"
     *                ~f:(fun p -> Rest.MarginPosition.sexp_of_t p |>
     *                             Sexplib.Sexp.to_string))
     *         end >>| fun () ->
     *         succ i
     *       end >>= function
     *       | 0 -> Logs_async.info (fun m -> m "No positions")
     *       | _ -> Deferred.unit
     *   end
     * | ["margin"] ->
     *   Rest.margin_account_summary ~key ~secret () >>= begin function
     *     | Error err -> Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
     *     | Ok resp ->
     *       Logs_async.info ~src (fun m -> m "%a" Sexp.pp (Rest.MarginAccountSummary.sexp_of_t resp))
     *   end
     * | ["balance"; currency]  ->
     *   Rest.balances ~key ~secret () >>= begin function
     *     | Error err ->
     *       Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
     *     | Ok resp ->
     *       Logs_async.info ~src (fun m -> m "found %d balances" @@ List.length resp) >>= fun () ->
     *       match List.Assoc.find ~equal:String.equal resp currency with
     *       | None -> Deferred.unit
     *       | Some b ->
     *         Logs_async.info ~src begin fun m ->
     *           m "%s: %a" currency Sexp.pp (Rest.Balance.sexp_of_t b)
     *         end
     *   end
     * | ["balances"]  ->
     *   Rest.positive_balances ~key ~secret () >>= begin function
     *     | Error err ->
     *       Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
     *     | Ok resp -> Deferred.List.iter resp ~f:begin fun (account, bs) ->
     *         Logs_async.info ~src begin fun m ->
     *           m "%a: %a"
     *             Sexp.pp (Rest.Account.sexp_of_t account)
     *             Sexp.pp Sexplib.Sexp.(List (List.map bs ~f:(fun (c, b) -> List [sexp_of_string c; sexp_of_float b])))
     *         end
     *       end
     *   end
     * | ["oos"] ->
     *   Rest.open_orders ~key ~secret () >>= begin function
     *     | Ok resp -> begin
     *         Deferred.List.fold ~init:0  resp ~f:begin fun i (s, oos) ->
     *           if oos <> [] then begin
     *             Logs_async.info ~src begin fun m ->
     *               m "%s: %a" s Sexp.pp Sexplib.(Std.sexp_of_list Rest.OpenOrder.sexp_of_t oos)
     *             end >>| fun () ->
     *             succ i
     *           end
     *           else Deferred.return i
     *         end >>= function
     *         | 0 ->
     *           Logs_async.info ~src (fun m -> m "No open orders")
     *         | _ -> Deferred.unit
     *       end
     *     | Error err ->
     *       Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
     *   end
     * | ["th"; symbol] ->
     *   Rest.trade_history ~symbol ~key ~secret () >>= begin function
     *     | Ok resp ->
     *       Deferred.List.iter resp ~f:begin fun (s, ths) ->
     *         Logs_async.info ~src begin fun m ->
     *           m "%s: %a" s Sexp.pp Sexplib.(Std.sexp_of_list Rest.TradeHistory.sexp_of_t ths)
     *         end
     *       end
     *     | Error err ->
     *       Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
     *   end
     * | ["cancel"; id] ->
     *   let order_id = Int.of_string id in
     *   Rest.cancel_order ~key ~secret ~order_id () >>= begin function
     *     | Ok () ->
     *       Logs_async.info ~src (fun m -> m "canceled order %d OK" order_id)
     *     | Error err ->
     *       Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
     *   end
     * | [side; symbol; price; qty] ->
     *   let side = Side.of_string side in
     *   let price = Float.of_string price in
     *   let qty = Float.of_string qty in
     *   if Plnx.margin_enabled symbol then
     *     Rest.submit_margin_order ~key ~secret ~symbol ~side ~price ~qty () >>= begin function
     *       | Ok resp -> Logs_async.info ~src (fun m -> m "%a" Sexp.pp (Rest.OrderResponse.sexp_of_t resp))
     *       | Error err -> Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
     *     end
     *   else
     *     Rest.submit_order ~key ~secret ~symbol ~side ~price ~qty () >>= begin function
     *       | Ok resp -> Logs_async.info ~src (fun m -> m "%a" Sexp.pp (Rest.OrderResponse.sexp_of_t resp))
     *       | Error err -> Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
     *     end
     * | ["modify"; id; price] ->
     *   let order_id = Int.of_string id in
     *   let price = Float.of_string price in
     *   Rest.modify_order ~key ~secret ~price ~order_id () >>= begin function
     *     | Ok resp -> Logs_async.info ~src (fun m -> m "%a" Sexp.pp (Rest.OrderResponse.sexp_of_t resp))
     *     | Error err -> Logs_async.err ~src (fun m -> m "%a" Rest.Http_error.pp err)
     *   end *)
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

let main () =
  (* let { Bs_devkit.Cfg.key ; secret ; _ } =
   *   List.Assoc.find_exn ~equal:String.equal cfg "PLNX" in *)
  Plnx_ws_async.with_connection_exn begin fun r w ->
    let log_incoming msg =
      Logs_async.debug ~src (fun m -> m "%a" pp msg) in
    Deferred.all_unit [
      process_user_cmd w ;
      Pipe.iter r ~f:log_incoming
    ]
  end

let () =
  Command.async ~summary:"Poloniex WS client" begin
    let open Command.Let_syntax in
    [%map_open
      (* let cfg = Bs_devkit.Cfg.param () *)
      let () = Logs_async_reporter.set_level_via_param None in
      fun () ->
        Logs.set_reporter (Logs_async_reporter.reporter ()) ;
        main ()
    ] end |>
  Command.run
