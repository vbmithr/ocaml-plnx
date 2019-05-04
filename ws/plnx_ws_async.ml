open Core
open Async

open Plnx
open Plnx_ws

let url = Uri.make ~scheme:"https" ~host:"api2.poloniex.com" ()

let connect ?(buf=Bi_outbuf.create 4096) () =
  Fastws_async.connect_ez url >>= fun (r, w, cleaned_up) ->
  let r = Pipe.map r ~f:(fun msg -> Yojson.Safe.from_string ~buf msg) in
  let r =
    Pipe.map r ~f:(fun msg -> Yojson_encoding.destruct_safe encoding msg) in
  let ws_read, client_write = Pipe.create () in
  don't_wait_for @@
  Pipe.transfer ws_read w ~f:begin fun cmd ->
    Yojson.Safe.to_string ~buf (Yojson_encoding.construct command_encoding cmd)
  end ;
  return (r, client_write, cleaned_up)

let with_connection ?(buf=Bi_outbuf.create 4096) f =
  Fastws_async.with_connection_ez url ~f:begin fun r w ->
    let r = Pipe.map r ~f:(fun msg -> Yojson.Safe.from_string ~buf msg) in
    let r =
      Pipe.map r ~f:(fun msg -> Yojson_encoding.destruct_safe encoding msg) in
    let ws_read, client_write = Pipe.create () in
    don't_wait_for @@
    Pipe.transfer ws_read w ~f:begin fun cmd ->
      Yojson.Safe.to_string ~buf (Yojson_encoding.construct command_encoding cmd)
    end ;
    f r client_write
  end
