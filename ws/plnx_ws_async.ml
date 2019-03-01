open Core
open Async

open Plnx
open Plnx_ws

let url = Uri.make ~scheme:"https" ~host:"api2.poloniex.com" ()
(* let url = Uri.make ~scheme:"http" ~host:"127.0.0.1" ~port:14443 () *)

let with_connection ?(buf=Bi_outbuf.create 4096) ?heartbeat f =
  let hb_ns = Option.map heartbeat ~f:Time_ns.Span.to_int63_ns in
  Fastws_async.with_connection_ez ?hb_ns url ~f:begin fun r w ->
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
