open Core
open Async

let url = Uri.make ~scheme:"https" ~host:"api2.poloniex.com" ()

(* let with_connection ?(buf=Bi_outbuf.create 4096) ?heartbeat f =
 *   let hb_ns = Option.map heartbeat ~f:Time_ns.Span.to_int63_ns in
 *   Fastws_async.with_connection_ez ?hb_ns url ~f:begin fun r w ->
 *     let rr = Pipe.map r ~f:begin fun msg ->
 *         Repr.of_yojson (Yojson.Safe.from_string ~buf msg)
 *       end in
 *     let ws_read, client_write = Pipe.create () in
 *     Pipe.transfer ws_read w ~f:begin function
 *       | Subscribe symbol ->
 *     end
 *   end *)
