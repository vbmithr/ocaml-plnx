module Encoding : sig
  val polo_fl : float Json_encoding.encoding
  (** [flstring] is an encoder for a float encoded as a string *)

  val polo_int : int64 Json_encoding.encoding
  (** [intstring] is an encoder for a int encoded as a string *)

  val polo_bool : bool Json_encoding.encoding
  (** [bool] is an encoder for a bool. *)

  val date : Ptime.t Json_encoding.encoding
  (** [date] is an encoder for Poloniex date (UNIX timestamp as string) *)
end

module Yojson_encoding : sig
  include module type of Json_encoding.Make(Json_repr.Yojson)
  val destruct_safe : 'a Json_encoding.encoding -> Yojson.Safe.t -> 'a
end

module Ptime : sig
  include module type of Ptime
    with type t = Ptime.t
     and type span = Ptime.span
end

module Side : sig
  type t = Fixtypes.Side.t [@@deriving sexp]

  val to_string : t -> string
  val of_string : string -> t

  val encoding : t Json_encoding.encoding
end

type time_in_force = [
  | `Fill_or_kill
  | `Immediate_or_cancel
]

module Ticker : sig
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

  val encoding : t Json_encoding.encoding
  val ws_encoding : t Json_encoding.encoding
end

module Trade : sig
  type t = {
    gid : int64 option ;
    orderNumber : int64 option ;
    id : int64 ;
    ts : Ptime.t ;
    side : Side.t ;
    price : float ;
    qty : float ;
  } [@@deriving sexp]

  val compare : t -> t -> int
  (** Uses price only *)

  val create :
    ?gid:int64 -> ?orderNumber:int64 -> id:int64 ->
    ts:Ptime.t -> side:Side.t ->
    price:float -> qty:float -> unit -> t

  val encoding : t Json_encoding.encoding
end

module BookEntry : sig
  type t = {
    price : float ;
    qty : float ;
  } [@@deriving sexp]

  val compare : t -> t -> int
  (** Uses price only *)

  val create : price:float -> qty:float -> t
end

module Pair : sig
  type t = {
    base: string ;
    quote: string ;
  } [@@deriving sexp]

  module Set   : Set.S with type elt := t
  module Map   : Map.S with type key := t
  module Table : Hashtbl.S with type key := t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
  val of_string : string -> t option
  val of_string_opt : string -> (t, string) result
  val of_string_exn : string -> t
  val encoding : t Json_encoding.encoding

end

val margin_enabled : string -> bool
