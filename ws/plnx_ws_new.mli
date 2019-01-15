open Core
open Async

open Plnx

module Repr : sig
  type snapshot = {
    symbol : string ;
    bid : Float.t Float.Map.t ;
    ask : Float.t Float.Map.t ;
  } [@@deriving sexp]

  type event =
    | Snapshot of snapshot
    | Update of BookEntry.t
    | Trade of Trade.t
  [@@deriving sexp]

  type t =
    | Error of string
    | Event of {
        subid : int ;
        id : int ;
        events : event list ;
      } [@@deriving sexp]

  type command =
    | Subscribe of string
end

val open_connection :
  ?heartbeat:Time_ns.Span.t ->
  ?connected:unit Condition.t ->
  ?disconnected:unit Condition.t ->
  Repr.command Pipe.Reader.t ->
  unit Condition.t * Repr.t Pipe.Reader.t
