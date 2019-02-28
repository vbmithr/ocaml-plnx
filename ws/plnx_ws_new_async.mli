open Core
open Async
open Plnx_ws_new

val with_connection :
  ?buf:Bi_outbuf.t ->
  ?heartbeat:Time_ns.Span.t ->
  (t Pipe.Reader.t -> command Pipe.Writer.t -> 'a Deferred.t) ->
  'a Deferred.t
