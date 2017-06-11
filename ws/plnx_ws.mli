open Core
open Async

open Plnx

type 'a msg = {
  typ: string ;
  data: 'a;
}

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

  val read_ticker : t -> Ticker.t
  val read_trade : t -> Trade.t
  val read_book : t -> Book.entry

  val of_msg : t msg -> t
  val to_msg : t -> (t msg, string) result
end

module M : S with type t := Msgpck.t

