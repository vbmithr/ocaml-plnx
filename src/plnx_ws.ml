open Core_kernel
open Plnx
open Json_encoding

type snapshot = {
  symbol : Pair.t ;
  bid : float Float.Map.t ;
  ask : float Float.Map.t ;
} [@@deriving sexp]

type event =
  | Err of string
  | Snapshot of snapshot
  | BookEntry of Side.t * BookEntry.t
  | Trade of Trade.t
  | Ticker of Ticker.t
[@@deriving sexp]

type t = {
  chanid : int ;
  seqnum : int ;
  events : event list ;
} [@@deriving sexp]

let empty events = { chanid = 0 ; seqnum = 0 ; events }

let pp ppf t =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp_hum (sexp_of_t t)

type creds = {
  key: string ;
  payload: string ;
  sign: string ;
}

let creds_encoding =
  conv
    (fun { key ; payload ; sign } -> key, payload, sign)
    (fun (key, payload, sign) -> { key ; payload ; sign })
    (obj3
       (req "key" string)
       (req "payload" string)
       (req "sign" string))

type channel =
  | Notifications of creds
  | Ticker
  | Volume
  | Heartbeat
  | TradesQuotes of Pair.t

let channel_of_int_exn = function
  | 1002 -> Ticker
  | 1003 -> Volume
  | 1010 -> Heartbeat
  | _ -> invalid_arg "channel_of_int_exn"

type command =
  | Subscribe of channel
  | Unsubscribe of int

let ticker = Subscribe Ticker
let volume = Subscribe Volume
let tradesQuotes pair = Subscribe (TradesQuotes pair)
let unsubscribe i = Unsubscribe i

let sub_encoding =
  conv (fun c -> (), c) (fun ((), c) -> c)
    (obj2
       (req "command" (constant "subscribe"))
       (req "channel" int))

let sub_str_encoding =
  conv (fun c -> (), c) (fun ((), c) -> c)
    (obj2
       (req "command" (constant "subscribe"))
       (req "channel" Pair.encoding))

let subscribe_encoding =
  union [
    case sub_encoding
      (function
        | Ticker -> Some 1002
        | Volume -> Some 1003
        | Heartbeat -> Some 1010
        | _ -> None) channel_of_int_exn ;
    case sub_str_encoding
      (function
        | TradesQuotes s -> Some s
        | _ -> None) (fun _ -> assert false) ;
    case (merge_objs sub_encoding creds_encoding)
      (function Notifications creds -> Some (1000, creds) | _ -> None)
      (fun (_, a) -> Notifications a)
  ]

let unsubscribe_encoding =
  conv
    (fun chanid -> (), chanid)
    (fun ((), chanid) -> chanid)
    (obj2
       (req "command" (constant "unsubscribe"))
       (req "channel" int))

let command_encoding =
  union [
    case subscribe_encoding
      (function Subscribe c -> Some c | _ -> None)
      (fun c -> Subscribe c) ;
    case unsubscribe_encoding
      (function Unsubscribe v -> Some v | _ -> None) (fun v -> Unsubscribe v) ;
  ]

let book_encoding =
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
  conv
    (fun { symbol ; bid ; ask } -> ((), (symbol, (bid, ask))))
    (fun ((), (symbol, (bid, ask))) -> { symbol ; bid ; ask })
    (tup2 (constant "i")
       (obj2
          (req "currencyPair" Pair.encoding)
          (req "orderBook" (tup2 book_encoding book_encoding))))

let side_encoding : Side.t Json_encoding.encoding =
  conv
    (function `buy -> 1 | `sell -> 0 | `buy_sell_unset -> 0)
    (function 1 -> `buy | 0 -> `sell | _ -> `buy_sell_unset) int

let ts_encoding =
  conv
    (fun t -> Int64.of_float (Ptime.to_float_s t))
    (fun i ->
       match Ptime.of_float_s (Int64.to_float i) with
       | None -> invalid_arg "ts_encoding"
       | Some ts -> ts)
    int53

let trade_encoding =
  let open Encoding in
  conv
    (fun { Trade.id ; ts ; side ; price ; qty ; _ } ->
       ((), id, side, price, qty, ts))
    (fun ((), id, side, price, qty, ts) ->
       Trade.create ~id ~side ~price ~qty ~ts ())
    (tup6
       (constant "t") polo_int side_encoding polo_fl polo_fl ts_encoding)

let order_encoding =
  let open Encoding in
  conv
    (fun (side, { BookEntry.price ; qty }) -> ((), side, price, qty))
    (fun ((), side, price, qty) -> side, { BookEntry.price ; qty })
    (tup4 (constant "o") side_encoding polo_fl polo_fl)

let event_encoding =
  union [
    case snapshot_encoding (function Snapshot s -> Some s | _ -> None) (fun s -> Snapshot s) ;
    case trade_encoding (function Trade t -> Some t | _ -> None) (fun t -> Trade t) ;
    case order_encoding (function BookEntry (s, e) -> Some (s, e) | _ -> None) (fun (s, e) -> BookEntry (s, e)) ;
  ]

let hello_encoding =
  conv
    (fun { chanid ; seqnum ; _ } -> (chanid, seqnum))
    (fun (chanid, seqnum) -> { chanid ; seqnum ; events = [] })
    (tup2 int int)

let msg_encoding =
  conv
    (fun { chanid ; seqnum ; events } -> (chanid, seqnum, events))
    (fun (chanid, seqnum, events) -> { chanid ; seqnum ; events })
    (tup3 int int (list event_encoding))

let ticker_encoding =
  conv
    (fun _ -> invalid_arg "not implemented")
    (fun (chanid, (), t) -> { chanid ; seqnum = 0 ; events = [ Ticker t ] })
    (tup3 int null Ticker.ws_encoding)

let hb_encoding =
  conv
    (fun _ -> invalid_arg "not implemented")
    (fun chanid -> { chanid ; seqnum = 0 ; events = [] })
    (tup1 int)

let error_encoding =
  obj1 (req "error" string)

let encoding =
  union [
    case error_encoding (fun _ -> None) (fun e -> empty [Err e]) ;
    case hello_encoding (fun _ -> None) (fun t -> t) ;
    case msg_encoding (fun t -> Some t) (fun t -> t) ;
    case ticker_encoding (fun t -> Some t) (fun t -> t) ;
    case hb_encoding (fun t -> Some t) (fun t -> t) ;
  ]
