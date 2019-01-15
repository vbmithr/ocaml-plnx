open Core
open Async

open Plnx

module Msg : sig
  type t =
    | Ticker of Ticker.t
    | Trade of Trade.t
    | BookModify of BookEntry.t
    | BookRemove of BookEntry.t

  val of_element : Wamp.Element.t -> t
end

module type S = sig
  type repr
  include Wamp.S with type repr := repr

  val open_connection :
    ?heartbeat:Time_ns.Span.t ->
    ?disconnected:unit Condition.t ->
    t Pipe.Reader.t ->
    t Pipe.Reader.t

  val subscribe :
    t Pipe.Writer.t -> string list -> int list Deferred.t
end

module M : S with type repr := Msgpck.t
module J : S with type repr := Yojson.Safe.json
