open Core
open Async

open Plnx

type currency = {
  id: int;
  name: string;
  txFee: string;
  minConf: int;
  depositAddress: string option;
  disabled: int;
  delisted: int;
  frozen: int;
}

type balance = {
  available: int;
  on_orders: int;
  btc_value: int;
}

type margin_account_summary = {
  total_value: int ;
  pl: int ;
  lending_fees: int ;
  net_value: int ;
  total_borrowed_value: int ;
  current_margin: float ;
}

module TradeHistory : sig
  type trade_category =
    | Exchange
    | Margin
    | Settlement
  [@@deriving sexp]

  type t = {
    gid: int;
    id: int;
    ts: Time_ns.t;
    price: int;
    qty: int;
    fee: int;
    order_id: int;
    side: Side.t;
    category: trade_category
  } [@@deriving sexp]

  val compare : t -> t -> int

  module Set : Set.S with type Elt.t = t
end

module OpenOrders : sig
  type t = {
    id: int;
    ts: Time_ns.t;
    side: Side.t;
    price: int;
    starting_qty: int;
    qty: int;
    margin: int;
  } [@@deriving sexp]

  val compare : t -> t -> int

  module Set : Set.S with type Elt.t = t
end

module MarginPosition : sig
  type position = {
    price: int;
    qty: int;
    total: int;
    pl: int;
    lending_fees: int;
    liquidation_price: int option;
    side: Side.t;
  }
  val position_encoding : position Json_encoding.encoding

  type t = {
    symbol : string ;
    position : position ;
  }

  val create : symbol:string -> position:position -> t
  val compare : t -> t -> int

  module Set : Set.S with type Elt.t = t
end

module Http_error : sig
  type t =
    | Cohttp of exn
    | Client of string
    | Server of string
    | Poloniex of string
    | Data_encoding of Yojson.Safe.json

  val to_string : t -> string
end

val margin_positions :
  ?buf:Bi_outbuf.t -> ?symbol:string ->
  key:string -> secret:Cstruct.t -> unit ->
  ((string * MarginPosition.position option) list, Http_error.t) Result.t Deferred.t
