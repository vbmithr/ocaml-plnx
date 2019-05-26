open Async
open Plnx_ws

val connect :
  ?buf:Bi_outbuf.t -> unit ->
  (t Pipe.Reader.t * command Pipe.Writer.t * unit Deferred.t) Deferred.t

val with_connection :
  ?buf:Bi_outbuf.t ->
  (t Pipe.Reader.t -> command Pipe.Writer.t -> 'a Deferred.t) ->
  'a Deferred.t
