open Sexplib.Std

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

  let string_float =
    conv Float.to_string Float.of_string string

  let string_bool =
    string_enum [
      "1", true ;
      "0", false
    ]

  let date =
    conv
      (fun _ -> invalid_arg "Encoding.date")
      (fun date ->
         match Ptime.of_rfc3339 (date ^ "Z") with
         | Error _ -> invalid_arg "invalid date"
         | Ok (t, _, _) -> t)
      string

  let string_int_or_int =
    union [
      case int (fun i -> Some i) (fun t -> t) ;
      case string (fun i -> Some (string_of_int i)) int_of_string
    ]
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
    symbol: string;
    last: float;
    ask: float;
    bid: float;
    pct_change: float;
    base_volume: float;
    quote_volume: float;
    is_frozen: bool;
    high24h: float;
    low24h: float;
  }

  let encoding ~symbol =
    let open Json_encoding in
    let open Encoding in
    conv
      (fun { symbol = _ ; last ; ask ; bid ; pct_change ; base_volume ;
             quote_volume ; is_frozen ; high24h ; low24h } ->
        (0, last, ask, bid, pct_change, base_volume,
         quote_volume, is_frozen, high24h, low24h))
      (fun (_id, last, ask, bid, pct_change, base_volume,
            quote_volume, is_frozen, high24h, low24h) ->
        { symbol ; last ; ask ; bid ; pct_change ; base_volume ;
          quote_volume ; is_frozen ; high24h ; low24h })
      (obj10
         (req "id" int)
         (req "last" string_float)
         (req "lowestAsk" string_float)
         (req "highestBid" string_float)
         (req "percentChange" string_float)
         (req "baseVolume" string_float)
         (req "quoteVolume" string_float)
         (req "isFrozen" string_bool)
         (req "high24hr" string_float)
         (req "low24hr" string_float))
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
      gid : int option ;
      id : int ;
      ts: Ptime.t ;
      side: Side.t ;
      price: float ;
      qty: float ;
    } [@@deriving sexp]

    let compare { price ; _ } { price = price' ; _ } = Float.compare price price'

    let create ?gid ~id ~ts ~side ~price ~qty () = {
      gid ; id ; ts ; side ; price ; qty
    }

    let encoding =
      let open Json_encoding in
      let open Encoding in
      conv
        (fun _ -> invalid_arg "Trade.construct not implemented")
        (fun (gid, id, date, side, price, qty, _total) ->
           let ts =
             match Ptime.(add_span date (Span.unsafe_of_d_ps (0, Int64.of_int id))) with
             | None -> invalid_arg "Trade.encoding"
             | Some t -> t in
           { gid ; id ; ts ; side ; price ; qty })
        (obj7
           (opt "globalTradeID" string_int_or_int)
           (req "tradeID" string_int_or_int)
           (req "date" date)
           (req "type" Side.encoding)
           (req "rate" string_float)
           (req "amount" string_float)
           (req "total" string_float))
  end
  include T
end

module BookEntry = struct
  module T = struct
    type t = {
      side : Side.t ;
      price : float ;
      qty : float ;
    } [@@deriving sexp]

    let compare { price ; _ } { price = price' ; _ } = Float.compare price price'

    let create ~side ~price ~qty = { side ; price ; qty }

    let encoding =
      let open Json_encoding in
      conv
        (fun { price ; side ; qty } -> (price, side, qty))
        (fun (price, side, qty) -> { price ; side ; qty })
        (obj3
           (req "rate" float)
           (req "type" Side.encoding)
           (dft "amount" float 0.))
  end
  include T
end

let flstring = Json_encoding.(Float.(conv to_string of_string string))
let intstring = Json_encoding.(conv string_of_int int_of_string string)
