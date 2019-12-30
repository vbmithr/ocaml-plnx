open Core_kernel
open Plnx

val url : Uri.t

type snapshot = {
  symbol : Pair.t ;
  bid : float Float.Map.t ;
  ask : float Float.Map.t ;
} [@@deriving sexp]

type event =
  | Err of string
  | Snapshot of snapshot
  | BookEntry of Side.t * BookEntry.t
  | Trade of Trade.t
  | Ticker of Ticker.t
[@@deriving sexp]

type t = {
  chanid : int ;
  seqnum : int ;
  events : event list ;
} [@@deriving sexp]

val pp : Format.formatter -> t -> unit
val encoding : t Json_encoding.encoding

type creds = {
  key: string ;
  payload: string ;
  sign: string ;
}

type channel =
  | Notifications of creds
  | Ticker
  | Volume
  | Heartbeat
  | TradesQuotes of Pair.t

type command =
  | Subscribe of channel
  | Unsubscribe of int

val ticker : command
val volume : command
val tradesQuotes : Pair.t -> command
val unsubscribe : int -> command

val command_encoding : command Json_encoding.encoding
val of_string : ?buf:Bi_outbuf.t -> string -> t
val string_of_command : ?buf:Bi_outbuf.t -> command -> string
