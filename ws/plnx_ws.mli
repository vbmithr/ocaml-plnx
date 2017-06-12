open Core
open Async

open Plnx

type msg =
  | Ticker of Ticker.t
  | Trade of Trade.t
  | BookModify of Book.entry
  | BookRemove of Book.entry

module type S = sig
  type t

  val open_connection :
    ?heartbeat:Time_ns.Span.t ->
    ?log_ws:Log.t ->
    ?log:Log.t ->
    ?disconnected:unit Condition.t ->
    t Wamp.msg Pipe.Reader.t ->
    t Wamp.msg Pipe.Reader.t

  val subscribe :
    t Wamp.msg Pipe.Writer.t -> string list -> int list Deferred.t

  val to_msg : t -> msg
end

module M : S with type t := Msgpck.t

