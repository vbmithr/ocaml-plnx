open Core
open Async

open Plnx
open Plnx_ws

module T = struct
  type t = {
    r: Plnx_ws.t Pipe.Reader.t ;
    w: Plnx_ws.command Pipe.Writer.t ;
  }

  let create r w = { r; w }

  module Address = Uri_sexp

  let is_closed { r; w } = Pipe.(is_closed r && is_closed w)
  let close { r; w } =
    Pipe.close w ;
    Pipe.close_read r ;
    Deferred.unit

  let close_finished { r; w } =
    Deferred.all_unit [Pipe.closed r;
                       Pipe.closed w]
end
include T

let mk_client_read ?buf r =
  Pipe.map r ~f:begin fun msg ->
    (Yojson_encoding.destruct_safe encoding
       (Yojson.Safe.from_string ?buf msg))
  end

let mk_client_write ?buf w =
  Pipe.create_writer begin fun ws_read ->
    Pipe.transfer ws_read w ~f:begin fun cmd ->
      Yojson.Safe.to_string ?buf
        (Yojson_encoding.construct command_encoding cmd)
    end
  end

let connect ?(buf=Bi_outbuf.create 4096) url =
  Deferred.Or_error.map (Fastws_async.EZ.connect url)
    ~f:begin fun { r; w; _ } ->
      let client_write = mk_client_write ~buf w in
      (Pipe.closed client_write >>> fun () -> Pipe.close w) ;
      create (mk_client_read ~buf r) client_write
    end

module Persistent = struct
  include Persistent_connection_kernel.Make(T)

  let create' ~server_name ?on_event ?retry_delay ?buf =
    create ~server_name ?on_event ?retry_delay
      ~connect:(connect ?buf)
end

let connect_exn ?buf url =
  connect ?buf url >>= function
  | Error e -> Error.raise e
  | Ok a -> return a

let with_connection ?(buf=Bi_outbuf.create 4096) ~f url =
  Fastws_async.EZ.with_connection url ~f:begin fun r w ->
    f (mk_client_read ~buf r) (mk_client_write ~buf w)
  end

let with_connection_exn ?buf ~f url =
  with_connection ?buf ~f url >>= function
  | Error e -> Error.raise e
  | Ok a -> return a
