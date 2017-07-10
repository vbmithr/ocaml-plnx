open Core
open Async

open Plnx

module type BACKEND = sig
  type repr

  include Wamp.S with type repr := repr

  val headers :
    Cohttp.Header.t

  val write :
    Log.t option -> Buffer.t -> string Pipe.Writer.t -> t -> unit Deferred.t

  val transfer :
    Log.t option -> string Queue.t -> t Queue.t Deferred.t
end

module Backend_msgpck = struct
  type repr = Msgpck.t

  include Wamp_msgpck

  let headers =
    Cohttp.Header.init_with "Sec-Websocket-Protocol" "wamp.2.msgpack.batched"

  let msg_size = Bytes.create 4

  let write log outbuf w msg =
    Buffer.clear outbuf;
    Buffer.add_bytes outbuf msg_size ;
    let nb_written = to_repr msg |> Msgpck.StringBuf.write outbuf in
    let serialized_msg = Buffer.contents outbuf in
    Binary_packing.pack_unsigned_32_int_big_endian serialized_msg 0 nb_written;
    Option.iter log ~f:(fun log -> Log.debug log "[WS] -> %S" serialized_msg) ;
    Pipe.write w serialized_msg

  let transfer log q =
    let res = Queue.create () in
    let rec read_loop pos msg_str =
      if pos < String.length msg_str then
        let nb_read, msg = Msgpck.String.read ~pos:(pos+4) msg_str in
        match of_repr msg with
        | Ok msg -> Queue.enqueue res msg; read_loop (pos+4+nb_read) msg_str
        | Error msg -> Option.iter log ~f:(fun log -> Log.error log "%s" msg)
    in
    Queue.iter q ~f:(read_loop 0);
    return res
end

module Backend_yojson = struct
  type repr = Yojson.Safe.json

  include Wamp_yojson

  let headers =
    Cohttp.Header.init_with "Sec-Websocket-Protocol" "wamp.2.json.batched"

  let write log outbuf w msg =
    Buffer.clear outbuf ;
    Buffer.add_string outbuf (to_repr msg |> Yojson.Safe.to_string) ;
    Buffer.add_char outbuf '\030' ;
    let contents = Buffer.contents outbuf in
    Option.iter log ~f:(fun log -> Log.debug log "[WS] -> %S" contents) ;
    Pipe.write w (Buffer.contents outbuf)

  let transfer log q =
    let res = Queue.create () in
    Queue.iter q ~f:begin fun str ->
      let msgs = String.split str ~on:'\030' in
      List.iter msgs ~f:begin function
        | "" -> ()
        | msg ->
          Option.iter log ~f:(fun log -> Log.debug log "<- %s" msg) ;
          match of_repr (Yojson.Safe.from_string msg) with
          | Ok msg -> Queue.enqueue res msg
          | Error msg -> Option.iter log ~f:(fun log -> Log.error log "%s" msg)
      end
    end ;
    return res
end

module Yojson_encoding = Json_encoding.Make(Json_repr.Yojson)

module Msg = struct
  type t =
    | Ticker of Ticker.t
    | Trade of Trade.t
    | BookModify of Book.entry
    | BookRemove of Book.entry

  let map_of_dict = function
    | Wamp.Element.Dict elts ->
      List.fold_left elts
        ~init:String.Map.empty ~f:(fun a (k, v) -> String.Map.add a k v)
    | _ -> invalid_arg "map_of_dict"

  let read_ticker = function
    | Wamp.Element.List [String symbol; String last; String ask; String bid; String pct_change;
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
    | _ -> invalid_arg "read_ticker"

  let read_trade msg = try
      let msg = map_of_dict msg in
      let id = String.Map.find_exn msg "tradeID" |> Wamp.Element.to_string |> Int.of_string in
      let date = String.Map.find_exn msg "date" |> Wamp.Element.to_string in
      let side = String.Map.find_exn msg "type" |> Wamp.Element.to_string in
      let rate = String.Map.find_exn msg "rate" |> Wamp.Element.to_string in
      let amount = String.Map.find_exn msg "amount" |> Wamp.Element.to_string in
      let ts = Time_ns.(add (of_string (date ^ "Z")) @@ Span.of_int_ns id) in
      let side = Side.of_string side in
      let price = Float.of_string rate in
      let qty = Float.of_string amount in
      Trade.create ~id ~ts ~side ~price ~qty ()
    with _ -> invalid_arg "read_trade"

  let read_book msg = try
      let msg = map_of_dict msg in
      let side = String.Map.find_exn msg "type" |> Wamp.Element.to_string in
      let price = String.Map.find_exn msg "rate" |> Wamp.Element.to_string in
      let qty = String.Map.find msg "amount" |> Option.map ~f:Wamp.Element.to_string in
      let side = Side.of_string side in
      let price = Float.of_string price in
      let qty = Option.value_map qty ~default:0. ~f:Float.of_string in
      Book.create_entry ~side ~price ~qty
    with _ -> invalid_arg "read_book"

  let of_element e =
    try
      let elts = map_of_dict e in
      let typ = String.Map.find_exn elts "type" |> Wamp.Element.to_string in
      let data = String.Map.find_exn elts "data" in
      match typ with
      | "newTrade" -> Trade (read_trade data)
      | "orderBookModify" -> BookModify (read_book data)
      | "orderBookRemove" -> BookRemove (read_book data)
      | _ -> invalid_arg "Plnx_ws.M.to_msg"
    with _ ->
      Ticker (read_ticker e)

  let ticker t = Ticker t
  let trade t = Trade t
  let book_modify entry = BookModify entry
  let book_remove entry = BookRemove entry
end

module Make (B : BACKEND) = struct
  include B

  let subscribe w topics =
    let topics = List.map topics ~f:Uri.of_string in
    Deferred.List.map ~how:`Sequential topics ~f:begin fun topic ->
      let request_id, subscribe_msg = EZ.subscribe topic in
      Pipe.write w subscribe_msg >>| fun () ->
      request_id
    end

  let open_connection
      ?(heartbeat=Time_ns.Span.of_int_sec 25)
      ?log_ws
      ?log
      ?disconnected
      to_ws =
    let uri_str = "https://api.poloniex.com" in
    let uri = Uri.of_string uri_str in
    let host = Option.value_exn ~message:"no host in uri" Uri.(host uri) in
    let port = Option.value_exn ~message:"no port inferred from scheme"
        Uri_services.(tcp_port_of_uri uri) in
    let scheme =
      Option.value_exn ~message:"no scheme in uri" Uri.(scheme uri) in
    let outbuf = Buffer.create 4096 in
    let rec loop_write mvar msg =
      Mvar.value_available mvar >>= fun () ->
      let w = Mvar.peek_exn mvar in
      if Pipe.is_closed w then begin
        Option.iter log ~f:(fun log -> Log.error log "loop_write: Pipe to websocket closed");
        Mvar.take mvar >>= fun _ ->
        loop_write mvar msg
      end
      else B.write log outbuf w msg
    in
    let ws_w_mvar = Mvar.create () in
    let ws_w_mvar_ro = Mvar.read_only ws_w_mvar in
    don't_wait_for @@
    Monitor.handle_errors begin fun () ->
      Pipe.iter ~continue_on_error:true to_ws ~f:(loop_write ws_w_mvar_ro)
    end
      (fun exn -> Option.iter log ~f:(fun log -> Log.error  log "%s" @@ Exn.to_string exn));
    let client_r, client_w = Pipe.create () in
    let process_ws r w =
      (* Initialize *)
      Option.iter log ~f:(fun log -> Log.info log "[WS] connected to %s" uri_str);
      let hello = EZ.(hello (Uri.of_string "realm1") [Subscriber]) in
      B.write log outbuf w hello >>= fun () ->
      Pipe.transfer' r client_w (B.transfer log)
    in
    let tcp_fun s r w =
      Socket.(setopt s Opt.nodelay true);
      begin
        if scheme = "https" || scheme = "wss" then
          Conduit_async_ssl.(ssl_connect (Ssl_config.configure ~version:Tlsv1_2 ()) r w)
        else return (r, w)
      end >>= fun (ssl_r, ssl_w) ->
      let ws_r, ws_w = Websocket_async.client_ez ?log:log_ws
          ~opcode:Binary ~extra_headers:B.headers ~heartbeat uri s ssl_r ssl_w
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
        Option.iter disconnected ~f:(fun c -> Condition.broadcast c ()) ;
        Option.iter log ~f:(fun log ->
            Log.error log "[WS] restarting connection to %s" uri_str);
        Clock_ns.after @@ Time_ns.Span.of_int_sec 10 >>=
        loop
      end
    in
    don't_wait_for @@ loop ();
    client_r
end

module type S = sig
  type repr
  include Wamp.S with type repr := repr

  val open_connection :
    ?heartbeat:Time_ns.Span.t ->
    ?log_ws:Log.t ->
    ?log:Log.t ->
    ?disconnected:unit Condition.t ->
    t Pipe.Reader.t ->
    t Pipe.Reader.t

  val subscribe :
    t Pipe.Writer.t -> string list -> int list Deferred.t
end

module M = Make(Backend_msgpck)
module J = Make(Backend_yojson)
