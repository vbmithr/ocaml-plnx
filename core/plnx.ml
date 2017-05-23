open Core

open Bs_devkit

type ticker = {
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

module Side = struct
  type t = [`Buy | `Sell] [@@deriving sexp]

  let to_string = function `Buy -> "buy" | `Sell -> "sell"

  let of_string = function
    | "buy" -> `Buy
    | "sell" -> `Sell
    | _ -> invalid_arg "side_of string"

  let encoding =
    let open Json_encoding in
    string_enum [
      "buy", `Buy ;
      "sell", `Sell ;
    ]
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

type trade = {
  ts: Time_ns.t;
  side: side;
  price: int; (* in satoshis *)
  qty: int; (* in satoshis *)
} [@@deriving sexp]

let get_tradeID = function
  | `Null -> None
  | `Int i -> Some i
  | `String s -> Option.some @@ Int.of_string s
  | #Yojson.Safe.json -> invalid_arg "get_tradeID"

let trade_encoding =
  let open Json_encoding in
  conv
    (fun { ts ; side ; price ; qty } ->
        let price = price // 100_000_000 in
        let qty = qty // 100_000_000 in
        let total = price *. qty in
        (None, Json_repr.to_any `Null,
         Time_ns.to_string ts, Side.to_string side,
         Float.to_string price,
         Float.to_string qty,
         Float.to_string total))
    (fun (globalTradeID, tradeID, date, typ, rate, amount, total) ->
       let tradeID = Json_repr.(any_to_repr (module Yojson) tradeID |> get_tradeID) in
       let id = Option.value ~default:0 tradeID in
       let ts = Time_ns.(add (of_string (date ^ "Z")) (Span.of_int_ns id)) in
       let side = Side.of_string typ in
       let price = satoshis_of_string rate in
       let qty = satoshis_of_string amount in
       { ts ; side ; price ; qty })
    (obj7
       (opt "globalTradeID" any_value)
       (req "tradeID" any_value)
       (req "date" string)
       (req "type" string)
       (req "rate" string)
       (req "amount" string)
       (req "total" string))


