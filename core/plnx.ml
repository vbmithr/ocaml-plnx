open Core

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
    conv
      (fun { symbol; last; ask ; bid ; pct_change ; base_volume ; quote_volume ;
             is_frozen ; high24h ; low24h } ->
        let open Float in
        let last = to_string last in
        let lowestAsk = to_string ask in
        let highestBid = to_string bid in
        let percentChange = to_string pct_change in
        let baseVolume = to_string base_volume in
        let quoteVolume = to_string quote_volume in
        let isFrozen = if is_frozen then "1" else "0" in
        let high24hr = to_string high24h in
        let low24hr = to_string low24h in
        (0, last, lowestAsk, highestBid, percentChange, baseVolume,
         quoteVolume, isFrozen, high24hr, low24hr))
      (fun (id, last, lowestAsk, highestBid, percentChange, baseVolume,
            quoteVolume, isFrozen, high24hr, low24hr) -> {
          symbol ;
          last = (Float.of_string last) ;
          ask = (Float.of_string lowestAsk) ;
          bid = (Float.of_string highestBid) ;
          pct_change = (Float.of_string percentChange) ;
          base_volume = (Float.of_string baseVolume) ;
          quote_volume = (Float.of_string quoteVolume) ;
          is_frozen = (match Int.of_string isFrozen with 0 -> false | _ -> true) ;
          high24h = (Float.of_string high24hr) ;
          low24h = (Float.of_string high24hr) })
      (obj10
         (req "id" int)
         (req "last" string)
         (req "lowestAsk" string)
         (req "highestBid" string)
         (req "percentChange" string)
         (req "baseVolume" string)
         (req "quoteVolume" string)
         (req "isFrozen" string)
         (req "high24hr" string)
         (req "low24hr" string))
end

module Side = struct
  type t = [`Buy | `Sell] [@@deriving sexp]

  let to_string = function `Buy -> "buy" | `Sell -> "sell"

  let of_string = function
    | "buy" -> `Buy
    | "sell" -> `Sell
    | "bid" -> `Buy
    | "ask" -> `Sell
    | _ -> invalid_arg "Side.of_string"

  let encoding =
    let open Json_encoding in
    string_enum [
      "buy", `Buy ;
      "bid", `Buy ;
      "sell", `Sell ;
      "ask", `Sell ;
    ]
end

type time_in_force = [
  | `Fill_or_kill
  | `Immediate_or_cancel
]

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
  type t = {
    gid : int option ;
    id : int ;
    ts: Time_ns.t ;
    side: Side.t ;
    price: float ;
    qty: float ;
  } [@@deriving sexp]

  let create ?gid ~id ~ts ~side ~price ~qty () = {
    gid ; id ; ts ; side ; price ; qty
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun _ -> (None, "", "", "", "", "", ""))
      (fun (gid, id, date, typ, rate, amount, total) ->
         let id = Int.of_string id in
         let ts = Time_ns.(add (of_string (date ^ "Z")) (Span.of_int_ns id)) in
         let side = Side.of_string typ in
         let price = Float.of_string rate in
         let qty = Float.of_string amount in
         { gid ; id ; ts ; side ; price ; qty })
      (obj7
         (opt "globalTradeID" int)
         (req "tradeID" string)
         (req "date" string)
         (req "type" string)
         (req "rate" string)
         (req "amount" string)
         (req "total" string))
end

module Book = struct
  type entry = {
    side : Side.t ;
    price : float ;
    qty : float ;
  } [@@deriving sexp]

  let create_entry ~side ~price ~qty = { side ; price ; qty }

  let encoding =
    let open Json_encoding in
    conv
      (fun { price ; side ; qty } ->
         let qty = if qty = 0. then None else Some qty in
         (price, side, qty))
      (fun (price, side, qty) ->
         { price ; side ; qty = Option.value ~default:0. qty })
      (obj3
         (req "rate" float)
         (req "type" Side.encoding)
         (opt "amount" float))
end

let flstring = Json_encoding.(Float.(conv to_string of_string string))

module Cfg = struct
  type cfg = {
    key: string ;
    secret: string ;
    passphrase: string [@default ""];
    quote: (string * int) list [@default []];
  } [@@deriving sexp]

  type t = (string * cfg) list [@@deriving sexp]
end
