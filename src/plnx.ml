open Sexplib.Std

module Yojson_encoding = struct
  include Json_encoding.Make(Json_repr.Yojson)

  let destruct_safe encoding value =
    try destruct encoding value with exn ->
      let value_str = Yojson.Safe.to_string value in
      Format.eprintf "%s@.%a@." value_str
        (Json_encoding.print_error ?print_unknown:None) exn ;
      raise exn
end

module Ptime = struct
  include Ptime

  let t_of_sexp sexp =
    let sexp_str = string_of_sexp sexp in
    match of_rfc3339 sexp_str with
    | Ok (t, _, _) -> t
    | _ -> invalid_arg "Ptime.t_of_sexp"

  let sexp_of_t t =
    sexp_of_string (to_rfc3339 t)
end

module Encoding = struct
  open Json_encoding

  let polo_fl =
    union [
      case float (fun a -> Some a) (fun a -> a) ;
      case string (fun a -> Some (string_of_float a)) (fun a -> float_of_string a) ;
    ]

  let polo_int =
    union [
      case int53 (fun i -> Some i) (fun t -> t) ;
      case string (fun i -> Some (Int64.to_string i)) Int64.of_string
    ]

  let polo_bool =
    union [
      case bool (fun b -> Some b) (fun t -> t) ;
      case int
        (function true -> Some 1 | false -> Some 0)
        (function 0 -> false | _ -> true) ;
      case string
        (function true -> Some "1" | false -> Some "0")
        (function "0" -> false | _ -> true) ;
    ]

  let date =
    conv
      (fun _ -> invalid_arg "Encoding.date")
      (fun date ->
         match Ptime.of_rfc3339 (date ^ "Z") with
         | Error _ -> invalid_arg "invalid date"
         | Ok (t, _, _) -> t)
      string
end

module Side = struct
  type t = [
    | `buy
    | `sell
    | `buy_sell_unset
  ] [@@deriving sexp]

  let to_string = function
    | `buy -> "buy"
    | `sell -> "sell"
    | `buy_sell_unset -> ""

  let of_string = function
    | "buy" -> `buy
    | "sell" -> `sell
    | "bid" -> `buy
    | "ask" -> `sell
    | _ -> invalid_arg "Side.of_string"

  let encoding =
    let open Json_encoding in
    string_enum [
      "buy", `buy ;
      "bid", `buy ;
      "sell", `sell ;
      "ask", `sell ;
    ]
end

type time_in_force = [
  | `Fill_or_kill
  | `Immediate_or_cancel
]

module Ticker = struct
  type t = {
    id: int ;
    last: float;
    ask: float;
    bid: float;
    pct_change: float;
    base_volume: float;
    quote_volume: float;
    is_frozen: bool;
    high24h: float;
    low24h: float;
  } [@@deriving sexp]

  let encoding =
    let open Json_encoding in
    let open Encoding in
    conv
      (fun { id ; last ; ask ; bid ; pct_change ; base_volume ;
             quote_volume ; is_frozen ; high24h ; low24h } ->
        (id, last, ask, bid, pct_change, base_volume,
         quote_volume, is_frozen, high24h, low24h))
      (fun (id, last, ask, bid, pct_change, base_volume,
            quote_volume, is_frozen, high24h, low24h) ->
        { id ; last ; ask ; bid ; pct_change ; base_volume ;
          quote_volume ; is_frozen ; high24h ; low24h })
      (obj10
         (req "id" int)
         (req "last" polo_fl)
         (req "lowestAsk" polo_fl)
         (req "highestBid" polo_fl)
         (req "percentChange" polo_fl)
         (req "baseVolume" polo_fl)
         (req "quoteVolume" polo_fl)
         (req "isFrozen" polo_bool)
         (req "high24hr" polo_fl)
         (req "low24hr" polo_fl))

  let ws_encoding =
    let open Json_encoding in
    let open Encoding in
    conv
      (fun { id ; last ; ask ; bid ; pct_change ; base_volume ;
             quote_volume ; is_frozen ; high24h ; low24h } ->
        (id, last, ask, bid, pct_change, base_volume,
         quote_volume, is_frozen, high24h, low24h))
      (fun (id, last, ask, bid, pct_change, base_volume,
            quote_volume, is_frozen, high24h, low24h) ->
        { id ; last ; ask ; bid ; pct_change ; base_volume ;
          quote_volume ; is_frozen ; high24h ; low24h })
      (tup10
         int polo_fl polo_fl polo_fl polo_fl polo_fl polo_fl
         polo_bool polo_fl polo_fl)
end

let margin_enabled = function
  | "BTC_XMR"
  | "BTC_ETH"
  | "BTC_CLAM"
  | "BTC_MAID"
  | "BTC_FCT"
  | "BTC_DASH"
  | "BTC_STR"
  | "BTC_BTS"
  | "BTC_LTC"
  | "BTC_XRP"
  | "BTC_DOGE" -> true
  | _ -> false

module Trade = struct
  module T = struct
    type t = {
      gid : int64 option ;
      orderNumber: int64 option ;
      id : int64 ;
      ts: Ptime.t ;
      side: Side.t ;
      price: float ;
      qty: float ;
    } [@@deriving sexp]

    let compare { price ; _ } { price = price' ; _ } = Float.compare price price'

    let create ?gid ?orderNumber ~id ~ts ~side ~price ~qty () = {
      gid ; id ; orderNumber ; ts ; side ; price ; qty
    }

    let encoding =
      let open Json_encoding in
      let open Encoding in
      conv
        (fun _ -> invalid_arg "Trade.construct not implemented")
        (fun (gid, orderNumber, id, date, side, price, qty, _total) ->
           let ts =
             match Ptime.(add_span date (Span.unsafe_of_d_ps (0, id))) with
             | None -> invalid_arg "Trade.encoding"
             | Some t -> t in
           { gid ; id ; orderNumber ; ts ; side ; price ; qty })
        (obj8
           (opt "globalTradeID" polo_int)
           (opt "orderNumber" polo_int)
           (req "tradeID" polo_int)
           (req "date" date)
           (req "type" Side.encoding)
           (req "rate" polo_fl)
           (req "amount" polo_fl)
           (req "total" polo_fl))
  end
  include T
end

module BookEntry = struct
  type t = {
    price : float ;
    qty : float ;
  } [@@deriving sexp]

  let compare { price ; _ } { price = price' ; _ } = Float.compare price price'

  let create ~price ~qty = { price ; qty }
end

module Pair = struct
  type t = {
    base: string ;
    quote: string ;
  } [@@deriving sexp]

  let compare { base ; quote } { base = base' ; quote = quote' } =
    match String.compare base base' with
    | 0 -> String.compare quote quote'
    | n -> n

  let pp ppf { base ; quote } =
    Format.fprintf ppf "%s_%s" quote base

  let to_string { base ; quote } =
    quote ^ "_" ^ base

  let of_string s =
    match String.split_on_char '_' s with
    | [quote ; base] -> Some { base ; quote }
    | _ -> None

  let of_string_exn s =
    match String.split_on_char '_' s with
    | [quote ; base] -> { base ; quote }
    | _ -> invalid_arg "pair_of_string_exn"
end
