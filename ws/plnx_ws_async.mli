open Async
open Plnx_ws

val with_connection :
  ?buf:Bi_outbuf.t ->
  (t Pipe.Reader.t -> command Pipe.Writer.t -> 'a Deferred.t) ->
  'a Deferred.t
