module Ptime : sig
  include module type of Ptime
    with type t = Ptime.t
     and type span = Ptime.span
end

module Side : sig
  type t = [
    | `buy
    | `sell
    | `buy_sell_unset
  ] [@@deriving sexp]

  val to_string : t -> string
  val of_string : string -> t

  val encoding : t Json_encoding.encoding
end

type time_in_force = [
  | `Fill_or_kill
  | `Immediate_or_cancel
]

val flstring : float Json_encoding.encoding
val intstring : int Json_encoding.encoding

module Ticker : sig
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

  val encoding : symbol:string -> t Json_encoding.encoding
end

module Trade : sig
  type t = {
    gid : int option ;
    id : int ;
    ts : Ptime.t ;
    side : Side.t ;
    price : float ;
    qty : float ;
  } [@@deriving sexp]

  val compare : t -> t -> int
  (** Uses price only *)

  val create :
    ?gid:int -> id:int -> ts:Ptime.t -> side:Side.t ->
    price:float -> qty:float -> unit -> t

  val encoding : t Json_encoding.encoding
end

module BookEntry : sig
  type t = {
    side : Side.t ;
    price : float ;
    qty : float ;
  } [@@deriving sexp]

  val compare : t -> t -> int
  (** Uses price only *)

  val create : side:Side.t -> price:float -> qty:float -> t

  val encoding : t Json_encoding.encoding
end

val margin_enabled : string -> bool
