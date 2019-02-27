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

  val pp : Format.formatter -> t -> unit

  type command =
    | Subscribe of string
end

val with_connection :
  ?heartbeat:Time_ns.Span.t ->
  (Repr.t Pipe.Reader.t * Repr.command Pipe.Writer.t -> unit Deferred.t) ->
  unit Deferred.t
