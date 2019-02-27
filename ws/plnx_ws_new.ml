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

  let pp ppf t =
    Format.fprintf ppf "%a" Sexplib.Sexp.pp_hum (sexp_of_t t)

  type command =
    | Subscribe of string

  let yojson_of_subscribe symbol =
    `Assoc [ "command", `String "subscribe" ; "channel", `String symbol ]

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

  let book_encoding =
    let open Json_encoding in
    conv begin fun m ->
      `O (List.map (Float.Map.to_alist m) ~f:begin fun (k, v) ->
          Float.to_string k, `String (Float.to_string v)
        end)
    end begin function
      | `O kvs ->
        List.fold_left kvs ~init:Float.Map.empty ~f:begin fun a -> function
          | key, `String data ->
            let key = Float.of_string key in
            let data = Float.of_string data in
            Float.Map.add_exn a ~key ~data
          | _, #Json_repr.ezjsonm -> failwith "book_encoding"
        end
      | #Json_repr.ezjsonm -> failwith "book_encoding"
    end
      any_ezjson_value

  let snapshot_encoding =
    let open Json_encoding in
    conv
      (fun { symbol ; bid ; ask } -> (symbol, (bid, ask)))
      (fun (symbol, (bid, ask)) -> { symbol ; bid ; ask })
      (obj2
         (req "currencyPair" string)
         (req "orderBook" (tup2 book_encoding book_encoding)))

  let side_encoding : Side.t Json_encoding.encoding =
    let open Json_encoding in
    conv
      (function `buy -> 1 | `sell -> 0 | `buy_sell_unset -> 0)
      (function 1 -> `buy | 0 -> `sell | _ -> `buy_sell_unset) int

  let ts_encoding =
    let open Json_encoding in
    conv
      (fun t -> Int64.of_float (Ptime.to_float_s t))
      (fun i ->
         match Ptime.of_float_s (Int64.to_float i) with
         | None -> invalid_arg "ts_encoding"
         | Some ts -> ts)
      int53

  let trade_encoding =
    let open Json_encoding in
    conv
      (fun { Trade.gid = _ ; id ; ts ; side ; price ; qty } ->
         ((), id, side, price, qty, ts))
      (fun ((), id, side, price, qty, ts) ->
         { Trade.id ; ts ; side ; price ; qty ; gid = None})
      (tup6
         (constant "t") intstring side_encoding flstring flstring ts_encoding)

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

let url = Uri.make ~scheme:"https" ~host:"api2.poloniex.com" ()

(* let with_connection ?(buf=Bi_outbuf.create 4096) ?heartbeat f =
 *   let hb_ns = Option.map heartbeat ~f:Time_ns.Span.to_int63_ns in
 *   Fastws_async.with_connection_ez ?hb_ns url ~f:begin fun r w ->
 *     let rr = Pipe.map r ~f:begin fun msg ->
 *         Repr.of_yojson (Yojson.Safe.from_string ~buf msg)
 *       end in
 *     let ws_read, client_write = Pipe.create () in
 *     Pipe.transfer ws_read w ~f:begin function
 *       | Subscribe symbol ->
 *     end
 *   end *)
