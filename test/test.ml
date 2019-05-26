open Core
open Async

module Cfg = struct
  type cfg = {
    key: string ;
    secret: string ;
    passphrase: string [@default ""];
    quote: (string * int) list [@default []];
  } [@@deriving sexp]

  type t = (string * cfg) list [@@deriving sexp]
end

let default_cfg = Filename.concat (Option.value_exn (Sys.getenv "HOME")) ".virtu"
let cfg =
  List.Assoc.find_exn ~equal:String.equal
    (Sexplib.Sexp.load_sexp_conv_exn default_cfg Cfg.t_of_sexp) "PLNX"

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Debug)

let wrap_request ?(speed=`Quick) n service =
  let auth = {
    Fastrest.key = cfg.Cfg.key ;
    secret = Base64.decode_exn cfg.Cfg.secret ;
    meta = [] ;
  } in
  Alcotest_async.test_case n speed begin fun () ->
    Fastrest.request ~auth service >>= function
    | Ok _v -> Deferred.unit
    | Error _ -> failwith ""
  end

let rest = [
  wrap_request "currencies" Plnx_rest.currencies ;
  wrap_request "tickers" Plnx_rest.tickers ;
  wrap_request "books" (Plnx_rest.books { base="ETH"; quote="BTC" }) ;
  Alcotest_async.test_case "trades" `Quick begin fun () ->
    Plnx_rest.trades { base="ETH"; quote="BTC" } >>= function
    | Ok v -> Pipe.drain v
    | Error e -> Alcotest.fail e
  end ;

  wrap_request "balances" (Plnx_rest.balances ()) ;
]

let () =
  Alcotest.run "plnx" [
    "rest", rest ;
  ]
