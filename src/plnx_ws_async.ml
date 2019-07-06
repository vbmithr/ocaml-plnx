open Core
open Async

open Plnx
open Plnx_ws

let url = Uri.make ~scheme:"https" ~host:"api2.poloniex.com" ()

let connect ?(buf=Bi_outbuf.create 4096) () =
  Fastws_async.connect_ez url >>|
  Result.map ~f:begin fun (r, w, cleaned_up) ->
    let ws_read, client_write = Pipe.create () in
    let client_read = Pipe.map r ~f:begin fun msg ->
        (Yojson_encoding.destruct_safe encoding
           (Yojson.Safe.from_string ~buf msg))
      end in
    don't_wait_for @@
    Pipe.transfer ws_read w ~f:begin fun cmd ->
      Yojson.Safe.to_string ~buf
        (Yojson_encoding.construct command_encoding cmd)
    end ;
    client_read, client_write, cleaned_up
  end

let connect_exn ?buf () =
  connect ?buf () >>= function
  | Error `Internal exn -> raise exn
  | Error `WS e -> Fastws_async.raise_error e
  | Ok a -> return a

let with_connection ?(buf=Bi_outbuf.create 4096) f =
  Fastws_async.with_connection_ez url ~f:begin fun r w ->
    let client_read = Pipe.map r ~f:begin fun msg ->
        Yojson_encoding.destruct_safe encoding (Yojson.Safe.from_string ~buf msg)
      end in
    let ws_read, client_write = Pipe.create () in
    don't_wait_for @@
    Pipe.transfer ws_read w ~f:begin fun cmd ->
      Yojson.Safe.to_string ~buf (Yojson_encoding.construct command_encoding cmd)
    end ;
    f client_read client_write
  end

let with_connection_exn ?buf f =
  with_connection ?buf f >>= function
  | Error `Internal exn
  | Error `User_callback exn -> raise exn
  | Error `WS e -> Fastws_async.raise_error e
  | Ok a -> return a
