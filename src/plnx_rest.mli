open Core
open Async

open Plnx

val of_ptime : Ptime.t -> Time_ns.t

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

module OpenOrder : sig
  type t = {
    id: int;
    ts: Time_ns.t;
    side: Side.t;
    price: float;
    starting_qty: float;
    qty: float;
    margin: int;
  } [@@deriving sexp]

  val create :
    id:int ->
    ts:Time_ns.t ->
    side:Side.t ->
    price:float ->
    starting_qty:float ->
    qty:float ->
    margin:int -> t

  val compare : t -> t -> int

  module Set : Set.S with type Elt.t = t
end

module MarginPosition : sig
  type t = {
    price: float;
    qty: float;
    total: float;
    pl: float;
    lending_fees: float;
    side: Side.t;
  } [@@deriving sexp]
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
    trades : (string * Plnx.Trade.t list) list;
    amount_unfilled : float ;
  } [@@deriving sexp]

  val trades_of_symbol :
    (string * Plnx.Trade.t list) list -> string -> Plnx.Trade.t list
end

module Books : sig
  type t = {
    asks: BookEntry.t list;
    bids: BookEntry.t list;
    isFrozen: bool;
    seq: int;
  }
end

val currencies : (Fastrest.form, (string * Currency.t) list) Fastrest.service
val tickers : (Fastrest.form, (Pair.t * Ticker.t) list) Fastrest.service
val books : ?depth:int -> Pair.t -> (Fastrest.form, Books.t) Fastrest.service

val trades :
  ?start_ts:Time_ns.t ->
  ?end_ts:Time_ns.t ->
  Pair.t ->
  (Plnx.Trade.t Pipe.Reader.t, string) Result.t Deferred.t

val all_trades :
  ?wait:Time_ns.Span.t ->
  ?start_ts:Time_ns.t ->
  ?end_ts:Time_ns.t -> Pair.t -> Plnx.Trade.t Pipe.Reader.t

val balances : ?all:bool -> unit ->
  (Fastrest.form, (string * Balance.t) list) Fastrest.service

(* val margin_positions :
 *   ?buf:Bi_outbuf.t ->
 *   key:string -> secret:string -> unit ->
 *   ((string * MarginPosition.t option) list, Http_error.t) Result.t Deferred.t
 * 
 * val margin_account_summary :
 *   ?buf:Bi_outbuf.t ->
 *   key:string -> secret:string -> unit ->
 *   (MarginAccountSummary.t, Http_error.t) Result.t Deferred.t
 * 
 * val open_orders :
 *   ?buf:Bi_outbuf.t -> ?symbol:string ->
 *   key:string -> secret:string -> unit ->
 *   ((string * OpenOrder.t list) list, Http_error.t) Result.t Deferred.t
 * 
 * val trade_history :
 *   ?buf:Bi_outbuf.t -> ?symbol:string ->
 *   ?start:Time_ns.t -> ?stop:Time_ns.t ->
 *   key:string -> secret:string -> unit ->
 *   ((string * TradeHistory.t list) list, Http_error.t) Result.t Deferred.t
 * 
 * val positive_balances :
 *   ?buf:Bi_outbuf.t ->
 *   key:string -> secret:string -> unit ->
 *   ((Account.t * (string * float) list) list, Http_error.t) Result.t Deferred.t
 * 
 * val submit_order :
 *   ?buf:Bi_outbuf.t -> ?tif:time_in_force -> ?post_only:bool ->
 *   key:string -> secret:string ->
 *   side:Side.t -> symbol:string -> price:float -> qty:float -> unit ->
 *   (OrderResponse.t, Http_error.t) result Deferred.t
 * 
 * val submit_margin_order :
 *   ?buf:Bi_outbuf.t -> ?tif:time_in_force ->
 *   ?post_only:bool -> ?max_lending_rate:float ->
 *   key:string -> secret:string ->
 *   side:Side.t -> symbol:string -> price:float -> qty:float -> unit ->
 *   (OrderResponse.t, Http_error.t) result Deferred.t
 * 
 * val cancel_order :
 *   ?buf:Bi_outbuf.t ->
 *   key:string -> secret:string ->
 *   order_id:int -> unit ->
 *   (unit, Http_error.t) result Deferred.t
 * 
 * val modify_order :
 *   ?buf:Bi_outbuf.t -> ?qty:float ->
 *   key:string -> secret:string ->
 *   price:float -> order_id:int -> unit ->
 *   (OrderResponse.t, Http_error.t) result Deferred.t *)
