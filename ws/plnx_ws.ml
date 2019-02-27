open Core_kernel
open Plnx

type snapshot = {
  symbol : string ;
  bid : float Float.Map.t ;
  ask : float Float.Map.t ;
} [@@deriving sexp]

type event =
  | Snapshot of snapshot
  | BookEntry of BookEntry.t
  | Trade of Trade.t
  | Ticker of Ticker.t
[@@deriving sexp]

type t = {
  chanid : int ;
  seqnum : int ;
  events : event list ;
} [@@deriving sexp]

let pp ppf t =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp_hum (sexp_of_t t)

type creds = {
  key: string ;
  payload: string ;
  sign: string ;
}

let id_or_string_encoding =
 let open Json_encoding in
  union [
    case string (function `String s -> Some s | `Id _ -> None) (fun s -> `String s) ;
    case int (function `String _ -> None | `Id i -> Some i) (fun i -> `Id i) ;
  ]

type command =
  | Subscribe of ([`String of string | `Id of int] * creds option)
  | Unsubscribe of [`String of string | `Id of int]

let subscribe_encoding =
  let open Json_encoding in
  conv
    (function
      | chanid, None -> (), chanid, None, None, None
      | chanid, Some { key ; payload ; sign } ->
        (), chanid, Some key, Some payload, Some sign)
    (fun ((), chanid, k, p, s) -> match k, p, s with
       | Some key, Some payload, Some sign ->
         chanid, Some { key ; payload ; sign }
       | _ -> chanid, None)
    (obj5
       (req "command" (constant "subscribe"))
       (req "channel" id_or_string_encoding)
       (opt "key" string)
       (opt "payload" string)
       (opt "sign" string))

let unsubscribe_encoding =
  let open Json_encoding in
  conv
    (fun chanid -> (), chanid)
    (fun ((), chanid) -> chanid)
    (obj2
       (req "command" (constant "unsubscribe"))
       (req "channel" id_or_string_encoding))

let command_encoding =
  let open Json_encoding in
  union [
    case subscribe_encoding
      (function Subscribe v -> Some v | _ -> None) (fun v -> Subscribe v) ;
    case unsubscribe_encoding
      (function Unsubscribe v -> Some v | _ -> None) (fun v -> Unsubscribe v) ;
  ]

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
    (fun { symbol ; bid ; ask } -> ((), (symbol, (bid, ask))))
    (fun ((), (symbol, (bid, ask))) -> { symbol ; bid ; ask })
    (tup2 (constant "i")
       (obj2
          (req "currencyPair" string)
          (req "orderBook" (tup2 book_encoding book_encoding))))

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
  let open Encoding in
  conv
    (fun { Trade.gid = _ ; id ; ts ; side ; price ; qty } ->
       ((), id, side, price, qty, ts))
    (fun ((), id, side, price, qty, ts) ->
       { Trade.id ; ts ; side ; price ; qty ; gid = None})
    (tup6
       (constant "t") polo_int side_encoding polo_fl polo_fl ts_encoding)

let order_encoding =
  let open Json_encoding in
  let open Encoding in
  conv
    (fun { BookEntry.side ; price ; qty } -> ((), side, price, qty))
    (fun ((), side, price, qty) -> { BookEntry.side ; price ; qty })
    (tup4 (constant "o") side_encoding polo_fl polo_fl)

let event_encoding =
  let open Json_encoding in
  union [
    case snapshot_encoding (function Snapshot s -> Some s | _ -> None) (fun s -> Snapshot s) ;
    case trade_encoding (function Trade t -> Some t | _ -> None) (fun t -> Trade t) ;
    case order_encoding (function BookEntry e -> Some e | _ -> None) (fun e -> BookEntry e) ;
  ]

let hello_encoding =
  let open Json_encoding in
  conv
    (fun { chanid ; seqnum ; _ } -> (chanid, seqnum))
    (fun (chanid, seqnum) -> { chanid ; seqnum ; events = [] })
    (tup2 int int)

let msg_encoding =
  let open Json_encoding in
  conv
    (fun { chanid ; seqnum ; events } -> (chanid, seqnum, events))
    (fun (chanid, seqnum, events) -> { chanid ; seqnum ; events })
    (tup3 int int (list event_encoding))

let ticker_encoding =
  let open Json_encoding in
  conv
    (fun _ -> invalid_arg "not implemented")
    (fun (chanid, (), t) -> { chanid ; seqnum = 0 ; events = [ Ticker t ] })
    (tup3 int null Ticker.ws_encoding)

let hb_encoding =
  let open Json_encoding in
  conv
    (fun _ -> invalid_arg "not implemented")
    (fun chanid -> { chanid ; seqnum = 0 ; events = [] })
    (tup1 int)

let encoding =
  let open Json_encoding in
  union [
    case hello_encoding (fun _ -> None) (fun t -> t) ;
    case msg_encoding (fun t -> Some t) (fun t -> t) ;
    case ticker_encoding (fun t -> Some t) (fun t -> t) ;
    case hb_encoding (fun t -> Some t) (fun t -> t) ;
  ]
