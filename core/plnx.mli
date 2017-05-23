open Core

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

module Side : sig
  type t = [`Buy | `Sell] [@@deriving sexp]

  val to_string : t -> string
  val of_string : string -> t

  val encoding : t Json_encoding.encoding
end

type trade = {
  ts: Time_ns.t;
  side: Side.t;
  price: int; (** in satoshis **)
  qty: int; (** in satoshis **)
} [@@deriving sexp]

val trade_encoding : trade Json_encoding.encoding

val get_tradeID : Yojson.Safe.json -> int option
