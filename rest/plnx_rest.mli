open Core
open Async

open Plnx

module Currency : sig
  type t = {
    id: int;
    name: string;
    txFee: string;
    minConf: int;
    depositAddress: string option;
    disabled: int;
    delisted: int;
    frozen: int;
  } [@@deriving sexp]
end

module Balance : sig
  type t = {
    available: float;
    on_orders: float;
    btc_value: float;
  } [@@deriving sexp]
end

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
    price: float;
    qty: float;
    fee: float;
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
    price: float;
    starting_qty: float;
    qty: float;
    margin: int;
  } [@@deriving sexp]

  val compare : t -> t -> int

  module Set : Set.S with type Elt.t = t
end

module MarginPosition : sig
  type position = {
    price: float;
    qty: float;
    total: float;
    pl: float;
    lending_fees: float;
    liquidation_price: float option;
    side: Side.t;
  } [@@deriving sexp]

  type t = {
    symbol : string ;
    position : position ;
  } [@@deriving sexp]

  val create : symbol:string -> position:position -> t
  val compare : t -> t -> int

  module Set : Set.S with type Elt.t = t
end

module MarginAccountSummary : sig
  type t = {
    total_value: float ;
    pl: float ;
    lending_fees: float ;
    net_value: float ;
    total_borrowed_value: float ;
    current_margin: float ;
  } [@@deriving sexp]

  val empty : t
end

module Account : sig
  type t =
    | Exchange
    | Margin
    | Lending [@@deriving sexp]
end

module OrderResponse : sig
  type t = {
    id : int ;
    trades : Trade.t list ;
    amount_unfilled : float ;
  } [@@deriving sexp]
end

module Http_error : sig
  type t =
    | Cohttp of exn
    | Client of string
    | Server of string
    | Poloniex of string
    | Data_encoding of string
    | Data_shape of string

  val to_string : t -> string
end

val currencies :
  ?buf:Bi_outbuf.t -> unit ->
  ((string * Currency.t) list, Http_error.t) Result.t Deferred.t

val symbols :
  ?buf:Bi_outbuf.t -> unit ->
  (string list, Http_error.t) Result.t Deferred.t

val tickers :
  ?buf:Bi_outbuf.t -> unit ->
  (Ticker.t list, Http_error.t) Result.t Deferred.t

val margin_positions :
  ?buf:Bi_outbuf.t -> ?symbol:string ->
  key:string -> secret:Cstruct.t -> unit ->
  ((string * MarginPosition.position) list, Http_error.t) Result.t Deferred.t

val margin_account_summary :
  ?buf:Bi_outbuf.t ->
  key:string -> secret:Cstruct.t -> unit ->
  (MarginAccountSummary.t, Http_error.t) Result.t Deferred.t

val open_orders :
  ?buf:Bi_outbuf.t -> ?symbol:string ->
  key:string -> secret:Cstruct.t -> unit ->
  ((string * OpenOrders.t list) list, Http_error.t) Result.t Deferred.t

val trade_history :
  ?buf:Bi_outbuf.t -> ?symbol:string ->
  ?start:Time_ns.t -> ?stop:Time_ns.t ->
  key:string -> secret:Cstruct.t -> unit ->
  ((string * TradeHistory.t list) list, Http_error.t) Result.t Deferred.t

val positive_balances :
  ?buf:Bi_outbuf.t ->
  key:string -> secret:Cstruct.t -> unit ->
  ((Account.t * (string * float) list) list, Http_error.t) Result.t Deferred.t

val balances :
  ?buf:Bi_outbuf.t -> ?all:bool ->
  key:string -> secret:Cstruct.t -> unit ->
  ((string * Balance.t) list, Http_error.t) Result.t Deferred.t

val order :
  ?buf:Bi_outbuf.t -> ?tif:time_in_force -> ?post_only:bool ->
  key:string -> secret:Cstruct.t ->
  side:Side.t -> symbol:string -> price:float -> qty:float -> unit ->
  (OrderResponse.t, Http_error.t) result Deferred.t

val margin_order :
  ?buf:Bi_outbuf.t -> ?tif:time_in_force ->
  ?post_only:bool -> ?max_lending_rate:float ->
  key:string -> secret:Cstruct.t ->
  side:Side.t -> symbol:string -> price:float -> qty:float -> unit ->
  (OrderResponse.t, Http_error.t) result Deferred.t

val cancel_order :
  ?buf:Bi_outbuf.t ->
  key:string -> secret:Cstruct.t ->
  order_id:int -> unit ->
  (unit, Http_error.t) result Deferred.t

val modify_order :
  ?buf:Bi_outbuf.t -> ?qty:float ->
  key:string -> secret:Cstruct.t ->
  price:float -> order_id:int -> unit ->
  (OrderResponse.t, Http_error.t) result Deferred.t

