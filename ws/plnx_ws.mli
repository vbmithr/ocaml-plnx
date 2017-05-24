open Core
open Async

open Plnx
open Bs_devkit

module M : sig
  val subscribe :
    Msgpck.t Wamp.msg Pipe.Writer.t -> string list -> int list Deferred.t

  val read_ticker : Msgpck.t -> Ticker.t
  val read_trade : Msgpck.t -> DB.trade
  val read_book : Msgpck.t -> DB.book_entry
end

type 'a t = {
  typ: string ;
  data: 'a;
}

val to_msgpck : Msgpck.t t -> Msgpck.t
val of_msgpck : Msgpck.t -> (Msgpck.t t, string) result

val open_connection :
  ?heartbeat:Time_ns.Span.t ->
  ?log_ws:Log.t ->
  ?log:Log.t ->
  Msgpck.t Wamp.msg Pipe.Reader.t ->
  Msgpck.t Wamp.msg Pipe.Reader.t
