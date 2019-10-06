open Async
open Plnx_ws

val connect :
  ?buf:Bi_outbuf.t -> unit ->
  (t Pipe.Reader.t * command Pipe.Writer.t * unit Deferred.t) Deferred.Or_error.t

val connect_exn :
  ?buf:Bi_outbuf.t -> unit ->
  (t Pipe.Reader.t * command Pipe.Writer.t * unit Deferred.t) Deferred.t

val with_connection :
  ?buf:Bi_outbuf.t ->
  (t Pipe.Reader.t -> command Pipe.Writer.t -> 'a Deferred.t) ->
  'a Deferred.Or_error.t

val with_connection_exn :
  ?buf:Bi_outbuf.t ->
  (t Pipe.Reader.t -> command Pipe.Writer.t -> 'a Deferred.t) ->
  'a Deferred.t
