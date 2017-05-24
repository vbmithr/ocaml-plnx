open Core
open Async

open Bs_devkit
open Plnx

let open_connection ?(heartbeat=Time_ns.Span.of_int_sec 25) ?log_ws ?log to_ws =
  let uri_str = "https://api.poloniex.com" in
  let uri = Uri.of_string uri_str in
  let host = Option.value_exn ~message:"no host in uri" Uri.(host uri) in
  let port = Option.value_exn ~message:"no port inferred from scheme"
      Uri_services.(tcp_port_of_uri uri) in
  let scheme =
    Option.value_exn ~message:"no scheme in uri" Uri.(scheme uri) in
  let outbuf = Buffer.create 4096 in
  let write_wamp w msg =
    Buffer.clear outbuf;
    Buffer.add_bytes outbuf @@ Bytes.create 4;
    let nb_written = Wamp_msgpck.msg_to_msgpck msg |> Msgpck.StringBuf.write outbuf in
    let serialized_msg = Buffer.contents outbuf in
    Binary_packing.pack_unsigned_32_int_big_endian serialized_msg 0 nb_written;
    (* Option.iter log ~f:(fun log -> Log.debug log "-> %s" (Wamp.sexp_of_msg Msgpck.sexp_of_t msg |> Sexplib.Sexp.to_string)); *)
    Pipe.write w serialized_msg
  in
  let rec loop_write mvar msg =
    Mvar.value_available mvar >>= fun () ->
    let w = Mvar.peek_exn mvar in
    if Pipe.is_closed w then begin
      Option.iter log ~f:(fun log -> Log.error log "loop_write: Pipe to websocket closed");
      Mvar.take mvar >>= fun _ ->
      loop_write mvar msg
    end
    else write_wamp w msg
  in
  let ws_w_mvar = Mvar.create () in
  let ws_w_mvar_ro = Mvar.read_only ws_w_mvar in
  don't_wait_for @@
  Monitor.handle_errors begin fun () ->
    Pipe.iter ~continue_on_error:true to_ws ~f:(loop_write ws_w_mvar_ro)
  end
    (fun exn -> Option.iter log ~f:(fun log -> Log.error  log "%s" @@ Exn.to_string exn));
  let transfer_f q =
    let res = Queue.create () in
    let rec read_loop pos msg_str =
      if pos < String.length msg_str then
        let nb_read, msg = Msgpck.String.read ~pos:(pos+4) msg_str in
        match Wamp_msgpck.msg_of_msgpck msg with
        | Ok msg -> Queue.enqueue res msg; read_loop (pos+4+nb_read) msg_str
        | Error msg -> Option.iter log ~f:(fun log -> Log.error log "%s" msg)
    in
    Queue.iter q ~f:(read_loop 0);
    return res
  in
  let client_r, client_w = Pipe.create () in
  let process_ws r w =
    (* Initialize *)
    Option.iter log ~f:(fun log -> Log.info log "[WS] connected to %s" uri_str);
    let hello = Wamp_msgpck.(hello (Uri.of_string "realm1") [Subscriber]) in
    write_wamp w hello >>= fun () ->
    Pipe.transfer' r client_w transfer_f
  in
  let tcp_fun s r w =
    Socket.(setopt s Opt.nodelay true);
    begin
      if scheme = "https" || scheme = "wss" then
        Conduit_async_ssl.ssl_connect ~version:Tlsv1_2 r w
      else return (r, w)
    end >>= fun (ssl_r, ssl_w) ->
    let extra_headers =
      Cohttp.Header.init_with "Sec-Websocket-Protocol" "wamp.2.msgpack.batched" in
    let ws_r, ws_w = Websocket_async.client_ez ?log:log_ws
        ~opcode:Binary ~extra_headers ~heartbeat uri s ssl_r ssl_w
    in
    let cleanup r w ws_r ws_w =
      Pipe.close_read ws_r ;
      Pipe.close ws_w ;
      Deferred.all_unit [
        Reader.close r ;
        Writer.close w ;
      ]
    in
    don't_wait_for begin
      Deferred.all_unit
        [ Reader.close_finished r ; Writer.close_finished w ] >>= fun () ->
      cleanup ssl_r ssl_w ws_r ws_w
    end ;
    Mvar.set ws_w_mvar ws_w ;
    process_ws ws_r ws_w
  in
  let rec loop () = begin
    Monitor.try_with_or_error ~name:"PNLX.Ws.open_connection"
      (fun () -> Tcp.(with_connection (to_host_and_port host port) tcp_fun)) >>| function
    | Ok () ->
      Option.iter log ~f:(fun log ->
          Log.error log "[WS] connection to %s terminated" uri_str)
    | Error err ->
      Option.iter log ~f:(fun log ->
          Log.error log "[WS] connection to %s raised %s" uri_str (Error.to_string_hum err))
  end >>= fun () ->
    if Pipe.is_closed client_r then Deferred.unit
    else begin
      Option.iter log ~f:(fun log ->
          Log.error log "[WS] restarting connection to %s" uri_str);
      Clock_ns.after @@ Time_ns.Span.of_int_sec 10 >>=
      loop
    end
  in
  don't_wait_for @@ loop ();
  client_r

module M = struct
  let map_of_msgpck = function
    | Msgpck.Map elts ->
      List.fold_left elts ~init:String.Map.empty ~f:begin fun a -> function
        | String k, v -> String.Map.add a k v
        | _ -> invalid_arg "map_of_msgpck"
      end
    | _ -> invalid_arg "map_of_msgpck"

  let subscribe w topics =
    let topics = List.map topics ~f:Uri.of_string in
    Deferred.List.map ~how:`Sequential topics ~f:begin fun topic ->
      let request_id, subscribe_msg = Wamp_msgpck.subscribe topic in
      Pipe.write w subscribe_msg >>| fun () ->
      request_id
    end

  let read_ticker = function
    | Msgpck.List [String symbol; String last; String ask; String bid; String pct_change;
                   String base_volume; String quote_volume; Int is_frozen; String high24h;
                   String low24h
                  ] ->
      let last = Float.of_string last in
      let ask = Float.of_string ask in
      let bid = Float.of_string bid in
      let pct_change = Float.of_string pct_change in
      let base_volume = Float.of_string base_volume in
      let quote_volume = Float.of_string quote_volume in
      let is_frozen = if is_frozen = 0 then false else true in
      let high24h = Float.of_string high24h in
      let low24h = Float.of_string low24h in
      { Ticker.symbol ; last ; ask ; bid ; pct_change ; base_volume ;
        quote_volume ; is_frozen ; high24h ; low24h }
    | _ -> invalid_arg "ticker_of_msgpck"

  let read_trade msg = try
      let msg = map_of_msgpck msg in
      let tradeID = String.Map.find_exn msg "tradeID" |> Msgpck.to_string |> Int.of_string in
      let date = String.Map.find_exn msg "date" |> Msgpck.to_string in
      let side = String.Map.find_exn msg "type" |> Msgpck.to_string in
      let rate = String.Map.find_exn msg "rate" |> Msgpck.to_string in
      let amount = String.Map.find_exn msg "amount" |> Msgpck.to_string in
      let ts = Time_ns.(add (of_string (date ^ "Z")) @@ Span.of_int_ns tradeID) in
      let side = match side with "buy" -> `Buy | "sell" -> `Sell | _ -> invalid_arg "typ_of_string" in
      let price = satoshis_of_string rate in
      let qty = satoshis_of_string amount in
      DB.{ ts ; side ; price ; qty }
    with _ -> invalid_arg "trade_of_msgpck"

  let read_book msg = try
      let msg = map_of_msgpck msg in
      let side = String.Map.find_exn msg "type" |> Msgpck.to_string in
      let price = String.Map.find_exn msg "rate" |> Msgpck.to_string in
      let qty = String.Map.find msg "amount" |> Option.map ~f:Msgpck.to_string in
      let side = match side with "bid" -> `Buy | "ask" -> `Sell | _ -> invalid_arg "book_of_book_raw" in
      let price = satoshis_of_string price in
      let qty = Option.value_map qty ~default:0 ~f:satoshis_of_string in
      DB.{ side ; price ; qty }
    with _ -> invalid_arg "book_of_msgpck"
end

(* module Yojson = struct *)
(*   type nonrec t = Yojson.Safe.json t *)
(*   let to_yojson = to_yojson Fn.id *)
(*   let of_yojson = of_yojson (fun json -> Ok json) *)

(*   let ticker_of_json = function *)
(*     | `List [`String symbol; `String last; `String ask; `String bid; `String pct_change; *)
(*              `String base_volume; `String quote_volume; `Int is_frozen; `String high24h; *)
(*              `String low24h *)
(*             ] -> *)
(*       let last = Float.of_string last in *)
(*       let ask = Float.of_string ask in *)
(*       let bid = Float.of_string bid in *)
(*       let pct_change = Float.of_string pct_change in *)
(*       let base_volume = Float.of_string base_volume in *)
(*       let quote_volume = Float.of_string quote_volume in *)
(*       let is_frozen = if is_frozen = 0 then false else true in *)
(*       let high24h = Float.of_string high24h in *)
(*       let low24h = Float.of_string low24h in *)
(*       { symbol ; last ; ask ; bid ; pct_change ; base_volume ; *)
(*         quote_volume ; is_frozen ; high24h ; low24h } *)
(*     | json -> invalid_argf "ticker_of_json: %s" Yojson.Safe.(to_string json) () *)

(*   type book_raw = { *)
(*     rate: string; *)
(*     typ: string ; *)
(*     amount: string option ; *)
(*   } *)

(*   let book_of_book_raw { rate; typ; amount } = *)
(*     let side = match typ with "bid" -> `Buy | "ask" -> `Sell | _ -> invalid_arg "book_of_book_raw" in *)
(*     let price = Fn.compose satoshis_int_of_float_exn Float.of_string rate in *)
(*     let qty = Option.value_map amount ~default:0 ~f:(Fn.compose satoshis_int_of_float_exn Float.of_string) in *)
(*     DB.{ side ; price ; qty } *)
(* end *)

type 'a t = {
  typ: string ;
  data: 'a;
}

let create ~typ ~data = { typ ; data }

let to_msgpck { typ; data } = Msgpck.(Map [String "type", String typ; String "data", data])
let of_msgpck elts =
  try
    let elts = M.map_of_msgpck elts in
    let typ = String.Map.find_exn elts "type" |> Msgpck.to_string in
    let data = String.Map.find_exn elts "data" in
    Result.return (create ~typ ~data)
  with exn -> Result.failf "%s" (Exn.to_string exn)

