open Core
open Async

open Plnx

let src = Logs.Src.create "plnx.ws"
    ~doc:"Poloniex API - Websocket library"

module Repr = struct
  type snapshot = {
    symbol : string ;
    bid : Float.t Float.Map.t ;
    ask : Float.t Float.Map.t ;
  } [@@deriving sexp]

  type event =
    | Snapshot of snapshot
    | Update of BookEntry.t
    | Trade of Trade.t
  [@@deriving sexp]

  type t =
    | Error of string
    | Event of {
        subid : int ;
        id : int ;
        events : event list ;
      } [@@deriving sexp]

  type command =
    | Subscribe of string

  let book_of_yojson book =
    let open Float in
    List.fold_left book ~init:Map.empty ~f:begin fun a -> function
      | price, `String qty -> Map.set a ~key:(of_string price) ~data:(of_string qty)
      | _ -> invalid_arg "Plnx_ws_new.book_of_yojson"
    end

  let side_of_int = function
    | 0 -> `sell
    | 1 -> `buy
    | _ -> invalid_arg "Repr.side_of_int"

  let event_of_yojson = function
    | `List [`String "i" ;
             `Assoc ["currencyPair", `String symbol ;
                     "orderBook", `List [`List [] ; `List []]]] ->
      Snapshot { symbol ; bid = Float.Map.empty ; ask = Float.Map.empty }
    | `List [`String "i" ;
             `Assoc ["currencyPair", `String symbol ;
                     "orderBook", `List [`Assoc ask ; `Assoc bid]]] ->
      Snapshot { symbol ; bid = book_of_yojson bid ; ask = book_of_yojson ask }
    | `List [`String "o" ; `Int side ; `String price ; `String qty ] ->
      let price = Float.of_string price in
      let qty = Float.of_string qty in
      let side = side_of_int side in
      Update (BookEntry.create ~side ~price ~qty)
    | `List [`String "t" ; `String id ; `Int side ; `String price ; `String qty ; `Int ts ] ->
      let id = Int.of_string id in
      let price = Float.of_string price in
      let qty = Float.of_string qty in
      let side = side_of_int side in
      let ts = Time_ns.of_int63_ns_since_epoch Int63.(of_int ts * of_int 1_000_000_000) in
      Trade (Trade.create ~id ~ts ~side ~price ~qty ())
    | `List [`String "t" ; `String id ; `Int side ; `String price ; `String qty ; `Intlit ts ] ->
      let id = Int.of_string id in
      let price = Float.of_string price in
      let qty = Float.of_string qty in
      let side = side_of_int side in
      let ts = Time_ns.of_int63_ns_since_epoch Int63.(of_string ts * of_int 1_000_000_000) in
      Trade (Trade.create ~id ~ts ~side ~price ~qty ())
    | #Yojson.Safe.t as json ->
      invalid_argf "Repr.event_of_yojson: %s" (Yojson.Safe.to_string json) ()

  let of_yojson = function
    | `Assoc [ "error", `String msg ] -> Error msg
    | `List [`Int subid] -> Event { subid ; id = 0 ; events = [] }
    | `List [`Int subid ; `Int id] -> Event { subid ; id ; events = [] }
    | `List [`Int subid ; `Int id ; `List events] ->
      Event { subid ; id ; events = List.map events ~f:event_of_yojson }
    | #Yojson.Safe.t as json ->
      invalid_argf "Repr.of_yojson: %s" (Yojson.Safe.to_string json) ()
end

let open_connection
    ?(heartbeat=Time_ns.Span.of_int_sec 25)
    ?connected
    ?disconnected
    to_ws =
  let buf = Bi_outbuf.create 1024 in
  let uri = Uri.make ~scheme:"https" ~host:"api2.poloniex.com" () in
  let rec loop_write mvar msg =
    Mvar.value_available mvar >>= fun () ->
    let w = Mvar.peek_exn mvar in
    if Pipe.is_closed w then begin
      Logs_async.err ~src begin fun m ->
        m "loop_write: Pipe to websocket closed"
      end >>= fun () ->
      Mvar.take mvar >>= fun _ ->
      loop_write mvar msg
    end
    else match msg with
      | Repr.Subscribe symbol ->
        Pipe.write w (Yojson.Safe.to_string ~buf (`Assoc [ "command", `String "subscribe" ;
                                                           "channel", `String symbol ]))
  in
  let ws_w_mvar = Mvar.create () in
  let ws_w_mvar_ro = Mvar.read_only ws_w_mvar in
  don't_wait_for @@
  Monitor.handle_errors begin fun () ->
    Pipe.iter ~continue_on_error:true to_ws ~f:(loop_write ws_w_mvar_ro)
  end
    (fun exn -> Logs.err ~src (fun m -> m "%a" Exn.pp exn));
  let client_r, client_w = Pipe.create () in
  let restart = Condition.create () in

  let cleanup r w ws_r ws_w =
    Pipe.close_read ws_r ;
    Pipe.close ws_w ;
    Deferred.all_unit [
      Reader.close r ;
      Writer.close w ;
    ] in

  let tcp_fun (_, r, w) =
    Option.iter connected ~f:(fun c -> Condition.broadcast c ()) ;
    let ws_r, ws_w = Websocket_async.client_ez
        ~opcode:Text ~heartbeat uri r w
    in
    don't_wait_for begin
      Deferred.all_unit
        [ Reader.close_finished r ;
          Writer.close_finished w ;
          Condition.wait restart ;
        ] >>= fun () ->
      cleanup r w ws_r ws_w
    end ;
    Mvar.set ws_w_mvar ws_w ;
    Logs_async.info ~src begin fun m ->
      m "connected to %a" Uri.pp_hum uri
    end >>= fun () ->
    Pipe.transfer ws_r client_w ~f:begin fun str ->
      Repr.of_yojson (Yojson.Safe.from_string ~buf str)
    end
  in
  let rec loop () = begin
    Monitor.try_with_or_error ~name:"Plnx_ws_new.open_connection"
      (fun () ->
         Conduit_async.V3.connect_uri uri >>= tcp_fun) >>= function
    | Ok () ->
      Logs_async.err ~src begin fun m ->
        m "connection to %a terminated" Uri.pp_hum uri
      end
    | Error err ->
      Logs_async.err ~src begin fun m ->
        m "connection to %a raised %a" Uri.pp_hum uri Error.pp err
      end
  end >>= fun () ->
    Option.iter disconnected ~f:(fun c -> Condition.broadcast c ()) ;
    if Pipe.is_closed client_r then Deferred.unit
    else begin
      Logs_async.err ~src begin fun m ->
        m "restarting connection to %a" Uri.pp_hum uri
      end >>= fun () ->
      Clock_ns.after @@ Time_ns.Span.of_int_sec 10 >>=
      loop
    end
  in
  don't_wait_for @@ loop ();
  restart, client_r
