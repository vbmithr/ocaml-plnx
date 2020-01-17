open Async

(* let auth =
 *   let key, secret =
 *     match String.split ~on:':' (Sys.getenv_exn "TOKEN_PLNX") with
 *     | [key; secret] -> key, secret
 *     | _ -> assert false in {
 *     Fastrest.key ;
 *     secret = Base64.decode_exn secret ;
 *     meta = [] ;
 *   } *)

let wrap_request ?(speed=`Quick) n service =
  Alcotest_async.test_case n speed begin fun () ->
    Deferred.ignore (Fastrest.request service)
  end

let rest = [
  wrap_request "currencies" Plnx_rest.currencies ;
  wrap_request "tickers" Plnx_rest.tickers ;
  wrap_request "books" (Plnx_rest.books { base="ETH"; quote="BTC" }) ;
  (* wrap_request "balances" (Plnx_rest.balances ()) ; *)
]

let () =
  Logs.set_reporter (Logs_async_reporter.reporter ()) ;
  Logs.set_level (Some Debug) ;
  Alcotest.run "plnx" [
    "rest", rest ;
  ]
