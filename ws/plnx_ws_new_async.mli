open Core
open Async

val with_connection :
  ?heartbeat:Time_ns.Span.t ->
  (Repr.t Pipe.Reader.t * Repr.command Pipe.Writer.t -> unit Deferred.t) ->
  unit Deferred.t
