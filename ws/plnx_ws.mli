open Core_kernel
open Plnx

type snapshot = {
  symbol : string ;
  bid : float Float.Map.t ;
  ask : float Float.Map.t ;
} [@@deriving sexp]

type event =
  | Snapshot of snapshot
  | BookEntry of BookEntry.t
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

type command =
  | Subscribe of ([`String of string | `Id of int] * creds option)
  | Unsubscribe of [`String of string | `Id of int]

val command_encoding : command Json_encoding.encoding
