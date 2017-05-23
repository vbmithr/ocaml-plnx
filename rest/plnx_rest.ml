open Core
open Async

open Bs_devkit
open Plnx

module Yojson_encoding = Json_encoding.Make(Json_repr.Yojson)

  open Cohttp_async

  exception Client of string
  exception Server of string
  exception Poloniex of string

  module Http_error = struct
    type t =
      | Cohttp of exn
      | Client of string
      | Server of string
      | Poloniex of string
      | Data_encoding of Yojson.Safe.json

    let poloniex_str msg = Poloniex msg
    let poloniex k =
      Format.kasprintf (fun msg -> Poloniex msg) k

    let poloniex_fail msg = Result.fail (Poloniex msg)

    let poloniex_failf k =
      Format.kasprintf (fun msg -> Result.fail (Poloniex msg)) k

    let data_encoding json = Result.fail (Data_encoding json)

    let to_string = function
      | Cohttp exn -> Exn.to_string exn
      | Client msg -> "HTTP Client error: " ^ msg
      | Server msg -> "HTTP Server error: " ^ msg
      | Poloniex msg -> "Poloniex error: " ^ msg
      | Data_encoding json -> "Data encoding error: " ^ (Yojson.Safe.to_string json)
  end

  let safe_get ?buf url =
    Monitor.try_with begin fun () ->
      Client.get url >>= fun (resp, body) ->
      let status_code = Cohttp.Code.code_of_status resp.status in
      Body.to_string body >>| fun body_str ->
      let body_json = Yojson.Safe.from_string ?buf body_str in
      if Cohttp.Code.is_client_error status_code then raise (Client body_str)
      else if Cohttp.Code.is_server_error status_code then raise (Server body_str)
      else match body_json with
        | `Assoc ["error", `String msg] -> raise (Poloniex msg)
        | #Yojson.Safe.json as json -> json
    end >>| Result.map_error ~f:begin function
      | Client str -> Http_error.Client str
      | Server str -> Server str
      | Poloniex str -> Poloniex str
      | exn -> Cohttp exn
    end

  let safe_post ?buf ~headers ~body url =
    Monitor.try_with begin fun () ->
      Client.post ~headers ~body url >>= fun (resp, body) ->
      let status_code = Cohttp.Code.code_of_status resp.status in
      Body.to_string body >>| fun body_str ->
      let body_json = Yojson.Safe.from_string ?buf body_str in
      if Cohttp.Code.is_client_error status_code then raise (Client body_str)
      else if Cohttp.Code.is_server_error status_code then raise (Server body_str)
      else match body_json with
        | `Assoc ["error", `String msg] -> raise (Poloniex msg)
        | #Yojson.Safe.json as json -> json
    end >>| Result.map_error ~f:begin function
      | Client str -> Http_error.Client str
      | Server str -> Server str
      | Poloniex str -> Poloniex str
      | exn -> Cohttp exn
    end

  let base_url = Uri.of_string "https://poloniex.com/public"
  let trading_url = Uri.of_string "https://poloniex.com/tradingApi"

  let ticker_encoding symbol =
    let open Json_encoding in
    conv
      (fun { symbol; last; ask ; bid ; pct_change ; base_volume ; quote_volume ;
             is_frozen ; high24h ; low24h } ->
        let open Float in
        let last = to_string last in
        let lowestAsk = to_string ask in
        let highestBid = to_string bid in
        let percentChange = to_string pct_change in
        let baseVolume = to_string base_volume in
        let quoteVolume = to_string quote_volume in
        let isFrozen = if is_frozen then "1" else "0" in
        let high24hr = to_string high24h in
        let low24hr = to_string low24h in
        (0, last, lowestAsk, highestBid, percentChange, baseVolume,
         quoteVolume, isFrozen, high24hr, low24hr))
      (fun (id, last, lowestAsk, highestBid, percentChange, baseVolume,
            quoteVolume, isFrozen, high24hr, low24hr) -> {
          symbol ;
          last = (Float.of_string last) ;
          ask = (Float.of_string lowestAsk) ;
          bid = (Float.of_string highestBid) ;
          pct_change = (Float.of_string percentChange) ;
          base_volume = (Float.of_string baseVolume) ;
          quote_volume = (Float.of_string quoteVolume) ;
          is_frozen = (match Int.of_string isFrozen with 0 -> false | _ -> true) ;
          high24h = (Float.of_string high24hr) ;
          low24h = (Float.of_string high24hr) })
      (obj10
         (req "id" int)
         (req "last" string)
         (req "lowestAsk" string)
         (req "highestBid" string)
         (req "percentChange" string)
         (req "baseVolume" string)
         (req "quoteVolume" string)
         (req "isFrozen" string)
         (req "high24hr" string)
         (req "low24hr" string))

  let tickers ?buf () =
    let url = Uri.with_query' base_url ["command", "returnTicker"] in
    safe_get ?buf url >>| Result.bind ~f:begin function
      | `Assoc tickers as json ->
        begin try
            Ok (List.rev_map tickers ~f:begin fun (symbol, t) ->
                Yojson_encoding.destruct (ticker_encoding symbol) t
              end)
          with exn -> Http_error.data_encoding json
        end
      | #Yojson.Safe.json as json -> Http_error.data_encoding json
    end

  let bids_asks_of_yojson side records =
    List.map records ~f:(function
      | `List [`String price; `Int qty] ->
        DB.{ side ; price = satoshis_of_string price ; qty = qty * 100_000_000 }
      | `List [`String price; `Float qty] ->
        DB.{ side ; price = satoshis_of_string price ; qty = satoshis_int_of_float_exn qty }
      | #Yojson.Safe.json -> invalid_arg "bids_asks_of_yojson")

  let bids_asks_of_yojson side = function
    | `List records -> bids_asks_of_yojson side records
    | #Yojson.Safe.json -> invalid_arg "bids_asks_of_yojson"

  let bids_of_yojson = bids_asks_of_yojson `Buy
  let asks_of_yojson = bids_asks_of_yojson `Sell

  type books = {
    asks: DB.book_entry list;
    bids: DB.book_entry list;
    isFrozen: bool;
    seq: int;
  }

  let books_encoding =
    let open Json_encoding in
    conv
      (fun { asks ; bids ; isFrozen ; seq } ->
         Json_repr.(repr_to_any (module Yojson) `Null),
         Json_repr.(repr_to_any (module Yojson) `Null),
         isFrozen, seq)
      (fun (asks, bids, isFrozen, seq) ->
         let asks = Json_repr.(any_to_repr (module Yojson) asks) |> asks_of_yojson in
         let bids = Json_repr.(any_to_repr (module Yojson) bids) |> bids_of_yojson in
         { asks ; bids ; isFrozen ; seq })
      (obj4
         (req "asks" any_value)
         (req "bids" any_value)
         (req "isFrozen" bool)
         (req "seq" int))

  let books ?buf ?depth symbol =
    let url = Uri.with_query' base_url @@ List.filter_opt [
        Some ("command", "returnOrderBook");
        Some ("currencyPair", symbol);
        Option.map depth ~f:(fun lvls -> "depth", Int.to_string lvls);
      ] in
    safe_get url >>| Result.bind ~f:begin fun json ->
      try
        Ok (Yojson_encoding.destruct books_encoding json)
      with exn -> Http_error.data_encoding json
    end

  let trades_exn ?log ?start_ts ?end_ts symbol =
    let start_ts_sec = Option.map start_ts ~f:begin fun ts ->
        Time_ns.to_int_ns_since_epoch ts / 1_000_000_000 |> Int.to_string
      end in
    let end_ts_sec = Option.map end_ts ~f:begin fun ts ->
        Time_ns.to_int_ns_since_epoch ts / 1_000_000_000 |> Int.to_string
      end in
    let url = Uri.add_query_params' base_url @@ List.filter_opt Option.[
        some ("command", "returnTradeHistory");
        some ("currencyPair", symbol);
        map start_ts_sec ~f:(fun t -> "start", t);
        map end_ts_sec ~f:(fun t -> "end", t);
      ]
    in
    let decoder = Jsonm.decoder `Manual in
    let fold_trades_exn trades_w (nb_decoded, name, tmp) chunk =
      let chunk_len = String.length chunk in
      let chunk = Caml.Bytes.unsafe_of_string chunk in
      Jsonm.Manual.src decoder chunk 0 chunk_len;
      let rec decode nb_decoded name tmp =
        match Jsonm.decode decoder with
        | `Error err ->
          let err_str = Format.asprintf "%a" Jsonm.pp_error err in
          Option.iter log ~f:(fun log -> Log.error log "%s" err_str) ;
          failwith err_str
        | `Lexeme (`Float f) -> decode nb_decoded "" ((name, `Int (Float.to_int f))::tmp)
        | `Lexeme (`String s) -> decode nb_decoded "" ((name, `String s)::tmp)
        | `Lexeme (`Name name) -> decode nb_decoded name tmp
        | `Lexeme `Oe ->
          let trade = Yojson_encoding.destruct trade_encoding (`Assoc tmp) in
          Pipe.write trades_w trade >>= fun () ->
          decode (succ nb_decoded) "" []
        | `Lexeme `Ae -> return (nb_decoded, name, tmp)
        | `Lexeme #Jsonm.lexeme -> decode nb_decoded name tmp
        | `Await -> return (nb_decoded, name, tmp)
        | `End -> return (nb_decoded, name, tmp)
      in
      decode nb_decoded name tmp
    in
    Option.iter log ~f:(fun log -> Log.debug log "GET %s" (Uri.to_string url)) ;
    Client.get url >>| fun (resp, body) ->
    Pipe.create_reader ~close_on_exception:false begin fun w ->
      let body_pipe = Body.to_pipe body in
      Deferred.ignore @@ Pipe.fold body_pipe
        ~init:(0, "", [])
        ~f:(fold_trades_exn w)
    end

  let all_trades_exn
      ?log
      ?(wait=Time_ns.Span.min_value)
      ?(start_ts=Time_ns.epoch)
      ?(end_ts=Time_ns.now ())
      symbol =
    let rec inner cur_end_ts w =
      trades_exn ?log ~end_ts:cur_end_ts symbol >>= fun trades ->
      let oldest_ts = ref @@ Time_ns.max_value in
      Pipe.transfer trades w ~f:(fun t -> oldest_ts := t.ts; t) >>= fun () ->
      if !oldest_ts = Time_ns.max_value || Time_ns.(!oldest_ts < start_ts) then
        (Pipe.close w; Deferred.unit)
      else
      Clock_ns.after wait >>= fun () ->
      inner Time_ns.(sub !oldest_ts @@ Span.of_int_sec 1) w
    in
    Pipe.create_reader ~close_on_exception:false (inner end_ts)

  type currency = {
    id: int;
    name: string;
    txFee: string;
    minConf: int;
    depositAddress: string option;
    disabled: int;
    delisted: int;
    frozen: int;
  }

  let currency_encoding =
    let open Json_encoding in
    conv
      (fun { id ; name ; txFee ; minConf ; depositAddress ;
             disabled ; delisted ; frozen } ->
        id, name, txFee, minConf, depositAddress, disabled, delisted, frozen)
      (fun (id, name, txFee, minConf, depositAddress, disabled, delisted, frozen) ->
         { id ; name ; txFee ; minConf ; depositAddress ;
           disabled ; delisted ; frozen })
      (obj8
         (req "id" int)
         (req "name" string)
         (req "txFee" string)
         (req "minConf" int)
         (opt "depositAddress" string)
         (req "disabled" int)
         (req "delisted" int)
         (req "frozen" int))

  let currencies ?buf () =
    let url = Uri.add_query_params' base_url ["command", "returnCurrencies"] in
    safe_get ?buf url >>| Result.bind ~f:begin function
      | `Assoc currs as json ->
        begin try
            Ok (List.map currs ~f:begin fun (code, obj) ->
                code, (Yojson_encoding.destruct currency_encoding obj)
              end)
          with _ -> Http_error.data_encoding json
        end
      | #Yojson.Safe.json -> Result.fail (Http_error.Poloniex "currencies")
    end

  let symbols ?buf () =
    let url =
      Uri.with_query' base_url ["command", "returnOrderBook";
                                "currencyPair", "all"; "depth", "0"] in
    safe_get ?buf url >>| Result.bind ~f:begin function
      | `Assoc syms -> Result.return @@ List.rev_map syms ~f:fst
      | #Yojson.Safe.json -> Http_error.poloniex_fail "symbols"
    end

  let make_sign () =
    let bigbuf = Bigstring.create 1024 in
    fun ~key ~secret ~data ->
      let nonce = Time_ns.(now () |> to_int_ns_since_epoch) / 1_000 in
      let data = ("nonce", [Int.to_string nonce]) :: data in
      let data_str = Uri.encoded_of_query data in
      let prehash = Cstruct.of_string ~allocator:(fun len -> Cstruct.of_bigarray bigbuf ~len) data_str in
      let `Hex signature = Nocrypto.Hash.SHA512.hmac ~key:secret prehash |> Hex.of_cstruct in
      data_str,
      Cohttp.Header.of_list [
        "content-type", "application/x-www-form-urlencoded";
        "Key", key;
        "Sign", signature;
      ]

  let sign = make_sign ()

(*   type balance_raw = { *)
(*     available: string; *)
(*     onOrders: string; *)
(*     btcValue: string; *)
(*   } *)

  type balance = {
    available: int;
    on_orders: int;
    btc_value: int;
  }

(*   let balance_of_balance_raw br = *)
(*     let on_orders = satoshis_of_string br.onOrders in *)
(*     let available = satoshis_of_string br.available in *)
(*     let btc_value = satoshis_of_string br.btcValue in *)
(*     create_balance ~available ~on_orders ~btc_value () *)

(*   let balances ?buf ?(all=true) ~key ~secret () = *)
(*     let data = List.filter_opt [ *)
(*         Some ("command", ["returnCompleteBalances"]); *)
(*         if all then Some ("account", ["all"]) else None *)
(*       ] *)
(*     in *)
(*     let data_str, headers = sign ~key ~secret ~data in *)
(*     Monitor.try_with_or_error begin fun () -> *)
(*       Client.post ~body:(Body.of_string data_str) ~headers trading_uri >>= fun (resp, body) -> *)
(*       Body.to_string body >>| fun body_str -> *)
(*       match Yojson.Safe.from_string ?buf body_str with *)
(*       | `Assoc ["error", `String msg] -> failwith msg *)
(*       | `Assoc balances -> List.Assoc.map balances ~f:begin fun b -> *)
(*           b |> balance_raw_of_yojson |> function *)
(*           | Ok br -> balance_of_balance_raw br *)
(*           | Error _ -> invalid_argf "balances: %s" body_str () *)
(*         end *)
(*       | json -> invalid_argf "balances: %s" body_str () *)
(*     end *)

(*   type account = Exchange | Margin | Lending [@@deriving sexp] *)

(*   let account_of_string = function *)
(*   | "exchange" -> Exchange *)
(*   | "margin" -> Margin *)
(*   | "lending" -> Lending *)
(*   | s -> invalid_argf "account_of_string: %s" s () *)

(*   let string_of_account = function *)
(*   | Exchange -> "exchange" *)
(*   | Margin -> "margin" *)
(*   | Lending -> "lending" *)

(*   let positive_balances ?buf ~key ~secret () = *)
(*     let invarg json = invalid_argf "positive_balances: %s" (Yojson.Safe.to_string ?buf json) () in *)
(*     let data = ["command", ["returnAvailableAccountBalances"]] in *)
(*     let data_str, headers = sign ~key ~secret ~data in *)
(*     Monitor.try_with_or_error begin fun () -> *)
(*       Client.post ~body:(Body.of_string data_str) ~headers trading_uri >>= fun (resp, body) -> *)
(*       Body.to_string body >>| fun body_str -> *)
(*       match Yojson.Safe.from_string ?buf body_str with *)
(*       | `Assoc ["error", `String msg] -> failwith msg *)
(*       | `Assoc balances -> List.map balances ~f:begin function *)
(*         | account, `Assoc bs -> *)
(*           account_of_string account, List.Assoc.map bs ~f:begin function *)
(*           | `String bal -> satoshis_of_string bal *)
(*           | json -> invarg json *)
(*           end *)
(*         | account, json -> invarg json *)
(*         end *)
(*       | json -> invarg json *)
(*     end *)

(*   type margin_account_summary_raw = { *)
(*     totalValue: string; *)
(*     pl: string; *)
(*     lendingFees: string; *)
(*     netValue: string; *)
(*     totalBorrowedValue: string; *)
(*     currentMargin: string; *)
(*   } *)

  type margin_account_summary = {
    total_value: int ;
    pl: int ;
    lending_fees: int ;
    net_value: int ;
    total_borrowed_value: int ;
    current_margin: float ;
  }

(*   let margin_account_summary_of_raw { totalValue; pl; lendingFees; netValue; *)
(*                                       totalBorrowedValue; currentMargin } = *)
(*     let total_value = satoshis_of_string totalValue in *)
(*     let pl = satoshis_int_of_float_exn @@ Float.of_string pl in *)
(*     let lending_fees = satoshis_int_of_float_exn @@ Float.of_string lendingFees in *)
(*     let net_value = satoshis_of_string netValue in *)
(*     let total_borrowed_value = satoshis_of_string totalBorrowedValue in *)
(*     let current_margin = Float.of_string currentMargin in *)
(*     create_margin_account_summary ~total_value ~pl ~lending_fees ~net_value *)
(*       ~total_borrowed_value ~current_margin () *)

(*   let margin_account_summary ?buf ~key ~secret () = *)
(*     let data = ["command", ["returnMarginAccountSummary"]] in *)
(*     let data_str, headers = sign ~key ~secret ~data in *)
(*     Monitor.try_with_or_error begin fun () -> *)
(*       Client.post ~body:(Body.of_string data_str) ~headers trading_uri >>= fun (resp, body) -> *)
(*       Body.to_string body >>| fun body_str -> *)
(*       match Yojson.Safe.from_string ?buf body_str |> margin_account_summary_raw_of_yojson with *)
(*       | Error msg -> failwith body_str *)
(*       | Ok mas_raw -> margin_account_summary_of_raw mas_raw *)
(*     end *)

(*   type order_response_raw = { *)
(*     success: int ; *)
(*     message: string ; *)
(*     error: string ; *)
(*     orderNumber: string ; *)
(*     resultingTrades: Yojson.Safe.json ; *)
(*     amountUnfilled: string ; *)
(*   } *)

(*   let fix_resultingTrades = function *)
(*   | `Null -> [] *)
(*   | `List resulting -> List.map resulting ~f:(fun tr -> trade_raw_of_yojson tr |> Result.ok_or_failwith) *)
(*   | `Assoc [_, `List resulting] -> List.map resulting ~f:(fun tr -> trade_raw_of_yojson tr |> Result.ok_or_failwith) *)
(*   | #Ezjsonm.value -> invalid_arg "fix_resultingTrades" *)

(*   type trade_info = { *)
(*     gid: int option; *)
(*     id: int; *)
(*     trade: trade; *)
(*   } *)

(*   type order_response = { *)
(*     id: int; *)
(*     trades: trade_info list; *)
(*     amount_unfilled: int; *)
(*   } *)

(*   (\* let order_response_encoding = *\) *)
(*   (\*   let open Json_encoding in *\) *)
(*   (\*   conv *\) *)
(*   (\*     (fun { id ; trades ; amount_unfilled } -> *\) *)
(*   (\*        (1, "", "", Int.to_string id, *\) *)
(*   (\*         Json_repr.to_any (`A []), Int.to_string amount_unfilled)) *\) *)
(*   (\*     (fun (success) *\) *)
(*   (\*     (obj6 *\) *)
(*   (\*        (dft "success" int 1) *\) *)
(*   (\*        (dft "message" string "") *\) *)
(*   (\*        (dft "error" string "") *\) *)
(*   (\*        (dft "orderNumber" string "") *\) *)
(*   (\*        (opt "resultingTrades" any_value) *\) *)
(*   (\*        (dft "amountUnfilled" string "")) *\) *)

(*   let trade_info_of_resultingTrades tr = *)
(*     let id = Option.value ~default:0 @@ get_tradeID tr.tradeID in *)
(*     let gid = get_tradeID tr.globalTradeID in *)
(*     let trade = trade_of_trade_raw tr in *)
(*     create_trade_info ?gid ~id ~trade () *)

(*   let order_response_of_raw { success; message; error; orderNumber; resultingTrades; amountUnfilled } = *)
(*     if success = 0 then Result.fail error *)
(*     else *)
(*     let id = Int.of_string orderNumber in *)
(*     let amount_unfilled = if amountUnfilled = "" then 0 else satoshis_of_string amountUnfilled in *)
(*     let trades = fix_resultingTrades resultingTrades in *)
(*     let trades = List.map trades ~f:trade_info_of_resultingTrades in *)
(*     Result.return @@ create_order_response ~id ~trades ~amount_unfilled () *)

(*   let order *)
(*       ?buf *)
(*       ?tif *)
(*       ?(post_only=false) *)
(*       ~key ~secret ~side ~symbol ~price ~qty () = *)
(*     let data = List.filter_opt [ *)
(*         Some ("command", [match side with `Buy -> "buy" | `Sell -> "sell" ]); *)
(*         Some ("currencyPair", [symbol]); *)
(*         Some ("rate", [Float.to_string @@ price // 100_000_000]); *)
(*         Some ("amount", [Float.to_string @@ qty // 100_000_000]); *)
(*         (match tif with *)
(*         | Some `Fill_or_kill -> Some ("fillOrKill", ["1"]) *)
(*         | Some `Immediate_or_cancel -> Some ("immediateOrCancel", ["1"]) *)
(*         | _ -> None); *)
(*         (if post_only then Some ("postOnly", ["1"]) else None) *)
(*       ] *)
(*     in *)
(*     let data_str, headers = sign ~key ~secret ~data in *)
(*     Monitor.try_with_or_error begin fun () -> *)
(*       Client.post ~body:(Body.of_string data_str) ~headers trading_uri >>= fun (resp, body) -> *)
(*       Body.to_string body >>| fun body_str -> *)
(*       Yojson.Safe.from_string ?buf body_str |> function *)
(*       | `Assoc ["error", `String msg] -> failwith msg (\* OK here! *\) *)
(*       | resp -> match order_response_raw_of_yojson resp with *)
(*       | Ok res -> order_response_of_raw res |> Result.ok_or_failwith *)
(*       | Error _ -> failwith body_str *)
(*     end *)

(*   type cancel_response_raw = { *)
(*     success: int ; *)
(*     amount: string ; *)
(*     message: string ; *)
(*     error: string ; *)
(*   } *)

(*   let cancel ?buf ~key ~secret id = *)
(*     let data = [ *)
(*         "command", ["cancelOrder"]; *)
(*         "orderNumber", [Int.to_string id]; *)
(*     ] *)
(*     in *)
(*     let data_str, headers = sign ~key ~secret ~data in *)
(*     Monitor.try_with_or_error begin fun () -> *)
(*       Client.post ~body:(Body.of_string data_str) ~headers trading_uri >>= fun (resp, body) -> *)
(*       Body.to_string body >>| fun body_str -> *)
(*       let resp = Yojson.Safe.from_string ?buf body_str in *)
(*       let resp = cancel_response_raw_of_yojson resp |> Result.ok_or_failwith in *)
(*       if resp.success = 1 then () else failwith resp.error *)
(*     end *)

(*   let modify ?buf ?qty ~key ~secret ~price id = *)
(*     let data = List.filter_opt [ *)
(*         Some ("command", ["moveOrder"]); *)
(*         Some ("orderNumber", [Int.to_string id]); *)
(*         Some ("rate", [price // 100_000_000 |> Float.to_string]); *)
(*         Option.map qty ~f:(fun a -> "amount", [a // 100_000_000 |> Float.to_string]) *)
(*       ] *)
(*     in *)
(*     let data_str, headers = sign ~key ~secret ~data in *)
(*     Monitor.try_with_or_error begin fun () -> *)
(*       Client.post ~body:(Body.of_string data_str) ~headers trading_uri >>= fun (resp, body) -> *)
(*       Body.to_string body >>| fun body_str -> *)
(*       Yojson.Safe.from_string ?buf body_str |> function *)
(*       | resp -> match order_response_raw_of_yojson resp with *)
(*       | Ok res -> order_response_of_raw res |> Result.ok_or_failwith *)
(*       | Error _ -> failwith body_str *)
(*     end *)

(*   let margin_order *)
(*       ?buf *)
(*       ?tif *)
(*       ?(post_only=false) *)
(*       ?max_lending_rate *)
(*       ~key ~secret ~side ~symbol ~price ~qty () = *)
(*     let data = List.filter_opt [ *)
(*         Some ("command", [match side with `Buy -> "marginBuy" | `Sell -> "marginSell" ]); *)
(*         Some ("currencyPair", [symbol]); *)
(*         Some ("rate", [Float.to_string @@ price // 100_000_000]); *)
(*         Some ("amount", [Float.to_string @@ qty // 100_000_000]); *)
(*         Option.map max_lending_rate ~f:(fun r -> "lendingRate", [Float.to_string r]); *)
(*         (match tif with *)
(*         | Some `Fill_or_kill -> Some ("fillOrKill", ["1"]) *)
(*         | Some `Immediate_or_cancel -> Some ("immediateOrCancel", ["1"]) *)
(*         | _ -> None); *)
(*         (if post_only then Some ("postOnly", ["1"]) else None) *)
(*       ] *)
(*     in *)
(*     let data_str, headers = sign ~key ~secret ~data in *)
(*     Monitor.try_with_or_error begin fun () -> *)
(*       Client.post ~body:(Body.of_string data_str) ~headers trading_uri >>= fun (resp, body) -> *)
(*       Body.to_string body >>| fun body_str -> *)
(*       Yojson.Safe.from_string ?buf body_str |> function *)
(*       | `Assoc ["error", `String msg] -> failwith msg (\* OK here! *\) *)
(*       | json -> match order_response_raw_of_yojson json with *)
(*       | Ok resp -> order_response_of_raw resp |> Result.ok_or_failwith *)
(*       | Error _ -> failwith body_str *)
(*     end *)

(*   let close_position ?buf ~key ~secret symbol = *)
(*     let data = [ *)
(*       "command", ["closeMarginPosition"]; *)
(*       "currencyPair", [symbol]; *)
(*     ] *)
(*     in *)
(*     let data_str, headers = sign ~key ~secret ~data in *)
(*     Monitor.try_with_or_error begin fun () -> *)
(*       Client.post ~body:(Body.of_string data_str) ~headers trading_uri >>= fun (resp, body) -> *)
(*       Body.to_string body >>| fun body_str -> *)
(*       let resp = Yojson.Safe.from_string ?buf body_str in *)
(*       match order_response_raw_of_yojson resp with *)
(*       | Ok resp -> order_response_of_raw resp |> Result.ok_or_failwith *)
(*       | Error _ -> failwith body_str *)
(*     end *)

(*   type open_orders_resp_raw = { *)
(*     orderNumber: string; *)
(*     typ: string ; *)
(*     rate: string; *)
(*     startingAmount: string; *)
(*     amount: string; *)
(*     total: string; *)
(*     date: string; *)
(*     margin: int; *)
(*   } *)

module OpenOrders = struct
  module T = struct
    type t = {
      id: int;
      ts: Time_ns.t;
      side: side;
      price: int;
      starting_qty: int;
      qty: int;
      margin: int;
    } [@@deriving sexp]

    let compare t t' = Int.compare t.id t'.id
  end
  include T
  module Set = Set.Make(T)
end

(*   let side_of_string = function *)
(*   | "buy" -> `Buy *)
(*   | "sell" -> `Sell *)
(*   | _ -> invalid_arg "side_of_string" *)

(*   let oo_of_oo_raw oo_raw = *)
(*     let id = Int.of_string oo_raw.orderNumber in *)
(*     let side = side_of_string oo_raw.typ in *)
(*     let price = satoshis_of_string oo_raw.rate in *)
(*     let qty = satoshis_of_string oo_raw.amount in *)
(*     let starting_qty = satoshis_of_string oo_raw.startingAmount in *)
(*     let ts = Time_ns.of_string (oo_raw.date ^ "Z") in *)
(*     let margin = oo_raw.margin in *)
(*     create_open_orders_resp ~id ~ts ~side ~price ~qty ~starting_qty ~margin () *)

(*   let open_orders ?buf ?(symbol="all") ~key ~secret () = *)
(*     let data = [ *)
(*       "command", ["returnOpenOrders"]; *)
(*       "currencyPair", [symbol]; *)
(*     ] *)
(*     in *)
(*     let data_str, headers = sign ~key ~secret ~data in *)
(*     let map_f oo = oo |> open_orders_resp_raw_of_yojson |> Result.ok_or_failwith |> oo_of_oo_raw in *)
(*     Monitor.try_with_or_error begin fun () -> *)
(*       Client.post ~body:(Body.of_string data_str) ~headers trading_uri >>= fun (resp, body) -> *)
(*       Body.to_string body >>| fun body_str -> *)
(*       Yojson.Safe.from_string ?buf body_str |> function *)
(*       | `Assoc ["error", `String msg] -> failwith msg *)
(*       | `List oos -> *)
(*         [symbol, List.map oos ~f:map_f] *)
(*       | `Assoc oo_assoc -> *)
(*         List.Assoc.map oo_assoc ~f:(function `List oos -> List.map oos ~f:map_f | #Yojson.Safe.json -> failwith body_str) *)
(*       | #Yojson.Safe.json -> failwith body_str *)
(*     end *)

  (* type trade_history_raw = { *)
  (*   globalTradeID: int; *)
  (*   tradeID: string; *)
  (*   date: string; *)
  (*   rate: string; *)
  (*   amount: string; *)
  (*   total: string; *)
  (*   fee: string; *)
  (*   orderNumber: string; *)
  (*   typ: string ; *)
  (*   category: string; *)
  (* } *)

module TradeHistory = struct
  type trade_category =
    | Exchange
    | Margin
    | Settlement [@@deriving sexp]

(*   let trade_category_of_string = function *)
(*   | "exchange" -> Exchange *)
(*   | "marginTrade" -> Margin *)
(*   | "settlement" -> Settlement *)
(*   | s -> invalid_argf "trade_category_of_string: %s" s () *)

  module T = struct
    type t = {
      gid: int;
      id: int;
      ts: Time_ns.t;
      price: int;
      qty: int;
      fee: int;
      order_id: int;
      side: side;
      category: trade_category
    } [@@deriving sexp]

    let compare t t' = Int.compare t.id t'.id
  end
  include T
  module Set = Set.Make(T)
end

(*   let trade_history_of_raw { globalTradeID; tradeID; date; rate; amount; total; fee; orderNumber; *)
(*                              typ; category } = *)
(*     let id = Int.of_string tradeID in *)
(*     let ts = Time_ns.of_string @@ date ^ "Z" in *)
(*     let price = satoshis_of_string rate in *)
(*     let qty = satoshis_of_string amount in *)
(*     let fee = satoshis_of_string fee in *)
(*     let order_id = Int.of_string orderNumber in *)
(*     let side = side_of_string typ in *)
(*     let category = trade_category_of_string category in *)
(*     create_trade_history ~gid:globalTradeID ~id ~ts ~price ~qty ~fee ~order_id ~side ~category () *)

(*   let trade_history ?buf ?(symbol="all") ?start ?stop ~key ~secret () = *)
(*     let data = List.filter_opt [ *)
(*         Some ("command", ["returnTradeHistory"]); *)
(*         Some ("currencyPair", [symbol]); *)
(*         Option.map start ~f:(fun ts -> "start", [Int.to_string @@ Time_ns.to_int_ns_since_epoch ts / 1_000_000_000]); *)
(*         Option.map stop ~f:(fun ts -> "end", [Int.to_string @@ Time_ns.to_int_ns_since_epoch ts / 1_000_000_000]); *)
(*     ] *)
(*     in *)
(*     let data_str, headers = sign ~key ~secret ~data in *)
(*     let map_f oo = oo |> trade_history_raw_of_yojson |> Result.ok_or_failwith |> trade_history_of_raw in *)
(*     Monitor.try_with_or_error begin fun () -> *)
(*       Client.post ~body:(Body.of_string data_str) ~headers trading_uri >>= fun (resp, body) -> *)
(*       Body.to_string body >>| fun body_str -> *)
(*       Yojson.Safe.from_string ?buf body_str |> function *)
(*       | `Assoc ["error", `String msg] -> failwith msg *)
(*       | `List ths -> *)
(*         [symbol, List.map ths ~f:map_f] *)
(*       | `Assoc oo_assoc -> *)
(*         List.Assoc.map oo_assoc ~f:(function `List oos -> List.map oos ~f:map_f | #Yojson.Safe.json -> failwith body_str) *)
(*       | #Yojson.Safe.json -> failwith body_str *)
(*     end *)

(*   type margin_position_raw = { *)
(*     amount: string; *)
(*     total: string; *)
(*     basePrice: string; *)
(*     liquidationPrice: Yojson.Safe.json; *)
(*     pl: string; *)
(*     lendingFees: string; *)
(*     typ: string ; *)
(*   } *)

module MarginPosition = struct
  type position = {
    price: float ;
    qty: float ;
    total: float ;
    pl: float ;
    lending_fees: float ;
    liquidation_price: float option ;
    side: side ;
  }

  type t = {
    symbol: string ;
    position : position ;
  }

  let create ~symbol ~position = { symbol ; position }
  let compare t t' = Pervasives.compare t t'

  let position_encoding =
    let open Json_encoding in
    conv
      (fun { price ; qty ; total ; pl ; lending_fees ; liquidation_price ; side } ->
         (price, qty, total, pl, lending_fees, liquidation_price, side))
      (fun (price, qty, total, pl, lending_fees, liquidation_price, side) ->
         { price ; qty ; total ; pl ; lending_fees ; liquidation_price ; side })
      (obj7
         (req "price" float)
         (req "qty" float)
         (req "total" float)
         (req "pl" float)
         (req "lending_fees" float)
         (opt "liquidation_price" float)
         (req "side" Side.encoding))
end

let margin_positions ?buf ?(symbol="all") ~key ~secret () =
  let data = ["command", ["getMarginPosition"]; "currencyPair", [symbol]] in
  let body, headers = sign ~key ~secret ~data in
  let body = Body.of_string body in
  (*FIXME: should this fail instead of filtering? *)
  let filter_map_f p =
    try Some (Yojson_encoding.destruct MarginPosition.position_encoding p) with _ -> None in
  safe_post ?buf ~headers ~body trading_url >>| Result.bind ~f:begin function
    | `Assoc (("type", _) :: a) as p -> Result.return [symbol, filter_map_f p]
    | `Assoc ps_assoc -> Result.return @@ List.Assoc.map ps_assoc ~f:filter_map_f
    | #Yojson.Safe.json as json -> Http_error.data_encoding json
  end

(*   let margin_position_of_raw { amount; total; basePrice; liquidationPrice; pl; lendingFees; typ } = *)
(*     let price = satoshis_of_string basePrice in *)
(*     let qty = satoshis_int_of_float_exn @@ Float.of_string amount in *)
(*     let total = satoshis_int_of_float_exn @@ Float.of_string total in *)
(*     let pl = satoshis_int_of_float_exn @@ Float.of_string pl in *)
(*     let lending_fees = satoshis_int_of_float_exn @@ Float.of_string lendingFees in *)
(*     let liquidation_price = match liquidationPrice with *)
(*     | `String price -> Option.some @@ satoshis_int_of_float_exn @@ Float.of_string price *)
(*     | #Yojson.Safe.json -> None *)
(*     in *)
(*     let side = match typ with | "long" -> Some `Buy | "short" -> Some `Sell | _ -> None in *)
(*     Option.map side ~f:begin fun side -> *)
(*       create_margin_position ~side ~price ~qty ~total ~pl ~lending_fees ?liquidation_price () *)
(*     end *)
