open Core
open Fastrest
open Plnx

module Yojson_encoding = struct
  include Json_encoding.Make(Json_repr.Yojson)

  let destruct_safe encoding value =
    try destruct encoding value with exn ->
      let value_str = Yojson.Safe.to_string value in
      Format.eprintf "%s@.%a@." value_str
        (Json_encoding.print_error ?print_unknown:None) exn ;
      raise exn
end

let latest_nonce =
  let open Int63 in
  ref (Time_ns.(to_int63_ns_since_epoch (now ())) / of_int 1_000)

let authf srv { Fastrest.key ; secret ; _ } =
  let ps = match srv.params with
    | Form ps -> ps
    | Json (_,_) -> assert false in
  let nonce = !latest_nonce in
  Int63.incr latest_nonce ;
  let params = ("nonce", [Int63.to_string nonce]) :: ps in
  let prehash = Uri.encoded_of_query params in
  let signature =
    Digestif.SHA512.(to_hex (hmac_string ~key:secret prehash)) in
  let headers = Httpaf.Headers.of_list [
      "Key", key;
      "Sign", signature;
    ] in
  { Fastrest.headers ; params = Form ps }

let base_url = Uri.make ~scheme:"https" ~host:"poloniex.com" ~path:"public" ()
let trading_url = Uri.make ~scheme:"https" ~host:"poloniex.com" ~path:"tradingApi" ()

let result_encoding encoding =
  let open Json_encoding in
  union [
    case (obj1 (req "error" (string)))
      (fun _ -> assert false)
      (fun e -> Error (Error.of_string e)) ;
    case encoding
      (function Error _ -> None | Ok v -> Some v)
      (fun v -> Ok v) ;
  ]

let depack_obj ~f encoding =
  let open Json_encoding in
  conv
    (fun _ -> invalid_arg "pack not supported")
    (fun a -> match Json_repr.(to_yojson (from_any a)) with
       | `Assoc vs ->
         List.map vs ~f:(fun (n, v) -> f n, Yojson_encoding.destruct_safe encoding v)
       | #Yojson.Safe.t -> invalid_arg "depack_obj")
    any_document

module Currency = struct
  type t = {
    id: int;
    name: string;
    txFee: string;
    minConf: int;
    depositAddress: string option;
    disabled: int;
    delisted: int;
    frozen: int;
  } [@@deriving sexp]

  let encoding =
    let open Json_encoding in
    let encoding =
      (obj8
         (req "id" int)
         (req "name" string)
         (req "txFee" string)
         (req "minConf" int)
         (req "depositAddress" (option string))
         (req "disabled" int)
         (req "delisted" int)
         (req "frozen" int)) in
    conv
      (fun { id ; name ; txFee ; minConf ; depositAddress ;
             disabled ; delisted ; frozen } ->
        ((), (id, name, txFee, minConf, depositAddress, disabled, delisted, frozen)))
      (fun ((), (id, name, txFee, minConf, depositAddress, disabled, delisted, frozen)) ->
         { id ; name ; txFee ; minConf ; depositAddress ;
           disabled ; delisted ; frozen })
      (merge_objs unit encoding)
end

let currencies =
  Fastrest.get (result_encoding (depack_obj ~f:Fn.id Currency.encoding))
    (Uri.with_query' base_url ["command", "returnCurrencies"])

let tickers = Fastrest.get (result_encoding (depack_obj ~f:Pair.of_string_exn Ticker.encoding))
    (Uri.with_query' base_url ["command", "returnTicker"])

module Books = struct
  type t = {
    asks: BookEntry.t list;
    bids: BookEntry.t list;
    isFrozen: bool;
    seq: int;
  }

  let be_encoding =
    let open Json_encoding in
    conv
      (fun { BookEntry.price ; qty } -> (price, qty))
      (fun (price, qty) -> { price ; qty })
      (tup2 Encoding.polo_fl float)

  let encoding =
    let open Json_encoding in
    conv
      (fun { asks ; bids ; isFrozen ; seq } -> (asks, bids, isFrozen, seq))
      (fun (asks, bids, isFrozen, seq) -> { asks ; bids ; isFrozen ; seq })
      (obj4
         (req "asks" (list be_encoding))
         (req "bids" (list be_encoding))
         (req "isFrozen" Encoding.polo_bool)
         (req "seq" int))
end

let books ?depth pair =
  Fastrest.get (result_encoding (Books.encoding))
    (Uri.with_query' base_url @@ List.filter_opt [
        Some ("command", "returnOrderBook");
        Some ("currencyPair", Pair.to_string pair);
        Option.map depth ~f:(fun lvls -> "depth", Int.to_string lvls);
      ])

module Balance = struct
  type t = {
    available: float;
    on_orders: float;
    btc_value: float;
  } [@@deriving sexp]

  let encoding =
    let open Json_encoding in
    let open Encoding in
    conv
      (fun _ -> (0., 0., 0.))
      (fun (available, on_orders, btc_value) -> { available ; on_orders ; btc_value })
      (obj3
         (req "available" polo_fl)
         (req "onOrders" polo_fl)
         (req "btcValue" polo_fl))
end

let balances ?(all=true) () =
  let params = List.filter_opt [
      Some ("command", ["returnCompleteBalances"]);
      if all then Some ("account", ["all"]) else None
    ] in
  Fastrest.post_form ~auth:authf ~params
    (result_encoding (depack_obj ~f:Fn.id Balance.encoding)) trading_url

(*   safe_post ?buf ~key ~secret ~data trading_url >>| Result.bind ~f:begin function
 *   | `Assoc balances -> begin
 *       try
 *         Result.return @@
 *         List.Assoc.map balances ~f:(Yojson_encoding.destruct_safe Balance.encoding)
 *       with exn -> Http_error.data_encoding exn
 *     end
 *   | #Yojson.Safe.t -> Result.fail (Http_error.Poloniex "balances")
 * end *)

module Account = struct
  type t =
    | Exchange
    | Margin
    | Lending [@@deriving sexp]

  let of_string = function
    | "exchange" -> Exchange
    | "margin" -> Margin
    | "lending" -> Lending
    | s -> invalid_argf "account_of_string: %s" s ()
end

(* let positive_balances ?buf ~key ~secret () =
 *   let data = ["command", ["returnAvailableAccountBalances"]] in
 *   safe_post ?buf ~key ~secret ~data trading_url >>| Result.bind ~f:begin function
 *     | `Assoc balances -> begin
 *         try
 *           Result.return @@
 *           List.map balances ~f:begin function
 *             | account, `Assoc bs ->
 *               Account.of_string account, List.Assoc.map bs ~f:begin function
 *                 | `String bal -> Float.of_string bal
 *                 | #Yojson.Safe.t -> raise Exit
 *               end
 *             | _, #Yojson.Safe.t -> raise Exit
 *           end
 *         with exn -> Http_error.data_encoding exn
 *       end
 *     | #Yojson.Safe.t -> Http_error.data_shape "expected object"
 *   end *)

module MarginAccountSummary = struct
  type t = {
    total_value: float ;
    pl: float ;
    lending_fees: float ;
    net_value: float ;
    total_borrowed_value: float ;
    current_margin: float ;
  } [@@deriving sexp]

  let empty = {
    total_value = 0. ;
    pl = 0. ;
    lending_fees = 0. ;
    net_value = 0. ;
    total_borrowed_value = 0. ;
    current_margin = 0.
  }

  let encoding =
    let open Json_encoding in
    conv
      (fun _ -> ("", "", "", "", "", ""))
      (fun (totalValue, pl, lendingFees, netValue,
            totalBorrowedValue, currentMargin) ->
        let total_value = Float.of_string totalValue in
        let pl = Float.of_string pl in
        let lending_fees = Float.of_string lendingFees in
        let net_value = Float.of_string netValue in
        let total_borrowed_value = Float.of_string totalBorrowedValue in
        let current_margin = Float.of_string currentMargin in
        { total_value ; pl ; lending_fees ; net_value ;
          total_borrowed_value ; current_margin })
      (obj6
         (req "totalValue" string)
         (req "pl" string)
         (req "lendingFees" string)
         (req "netValue" string)
         (req "totalBorrowedValue" string)
         (req "currentMargin" string))
end

(* let margin_account_summary ?buf ~key ~secret () =
 *   let data = ["command", ["returnMarginAccountSummary"]] in
 *   safe_post ?buf ~key ~secret ~data trading_url >>| Result.bind ~f:begin fun json ->
 *     Ok (Yojson_encoding.destruct_safe MarginAccountSummary.encoding json)
 *   end
 * 
 * let or_error encoding =
 *   let open Json_encoding in
 *   let error_encoding =
 *     conv
 *       (fun _ -> ((), ""))
 *       (fun ((), error) -> Http_error.Poloniex error)
 *       (merge_objs unit (obj1 (req "error" string))) in
 *   union [
 *     case error_encoding
 *       (function Ok _ -> None | Error msg -> Some msg)
 *       (fun msg -> Error msg) ;
 *     case encoding
 *       (function Ok v -> Some v | Error _ -> None)
 *       (fun v -> Ok v)
 *   ] *)

module OrderResponse = struct
  type t = {
    id : int ;
    trades : (string * Trade.t list) list ;
    amount_unfilled : float ;
  } [@@deriving sexp]

  let trades_of_symbol trades symbol =
    match List.Assoc.find trades ~equal:String.equal symbol with
    | Some trades -> trades
    | None -> List.Assoc.find_exn trades ~equal:String.equal ""

  let encoding =
    let open Json_encoding in
    let open Encoding in
    conv
      (fun _ -> invalid_arg "Order_response.encoding: not implemented")
      (fun ((), (id, trades, amount_unfilled)) ->
         let id = Int.of_string id in
         let amount_unfilled = Option.value amount_unfilled ~default:0. in
         let trades =
           match Json_repr.(any_to_repr (module Yojson) trades) with
           | `Assoc assc ->
             List.map assc ~f:begin fun (symbol, trades) ->
               symbol,
               Yojson_encoding.destruct_safe (list Trade.encoding) trades
             end
           | `List l ->
             List.map l ~f:begin fun trades ->
               "",
               Yojson_encoding.destruct_safe (list Trade.encoding) trades
             end
           | #Yojson.Safe.t as json ->
             invalid_argf "OrderResponse: %s" (Yojson.Safe.to_string json) ()
         in
         { id ; trades ; amount_unfilled })
      (merge_objs unit
         (obj3
            (req "orderNumber" string)
            (dft "resultingTrades" any_value Json_repr.(repr_to_any (module Yojson) (`List [])))
            (opt "amountUnfilled" polo_fl)))
end

(* let submit_order ?buf ?tif ?(post_only=false)
 *     ~key ~secret ~side ~symbol ~price ~qty () =
 *   let data = List.filter_opt [
 *       Some ("command", [Side.to_string side]);
 *       Some ("currencyPair", [symbol]);
 *       Some ("rate", [Float.to_string price]);
 *       Some ("amount", [Float.to_string qty]);
 *       (match tif with
 *        | Some `Fill_or_kill -> Some ("fillOrKill", ["1"])
 *        | Some `Immediate_or_cancel -> Some ("immediateOrCancel", ["1"])
 *        | _ -> None);
 *       (if post_only then Some ("postOnly", ["1"]) else None)
 *     ] in
 *   safe_post ?buf ~key ~secret ~data trading_url >>|
 *   Result.bind ~f:(Yojson_encoding.destruct_safe (or_error OrderResponse.encoding))
 * 
 * let cancel_order ?buf ~key ~secret ~order_id () =
 *   let response_encoding = or_error Json_encoding.unit in
 *   let data = [
 *     "command", ["cancelOrder"];
 *     "orderNumber", [Int.to_string order_id];
 *   ] in
 *   safe_post ?buf ~key ~secret ~data trading_url >>|
 *   Result.bind ~f:(Yojson_encoding.destruct_safe response_encoding)
 * 
 * let modify_order ?buf ?qty ~key ~secret ~price ~order_id () =
 *   let data = List.filter_opt [
 *       Some ("command", ["moveOrder"]);
 *       Some ("orderNumber", [Int.to_string order_id]);
 *       Some ("rate", [Float.to_string price]);
 *       Option.map qty ~f:(fun amount -> "amount", [Float.to_string amount])
 *     ] in
 *   safe_post ?buf ~key ~secret ~data trading_url >>|
 *   Result.bind ~f:(Yojson_encoding.destruct_safe (or_error OrderResponse.encoding))
 * 
 * let submit_margin_order
 *     ?buf
 *     ?tif
 *     ?(post_only=false)
 *     ?max_lending_rate
 *     ~key ~secret ~side ~symbol ~price ~qty () =
 *   let side =
 *     match side with
 *     | `buy_sell_unset -> invalid_arg "Plnx_rest.margin_order: side unset"
 *     | `buy -> "marginBuy"
 *     | `sell -> "marginSell" in
 *   let data = List.filter_opt [
 *       Some ("command", [side]);
 *       Some ("currencyPair", [symbol]);
 *       Some ("rate", [Float.to_string price]);
 *       Some ("amount", [Float.to_string qty]);
 *       Option.map max_lending_rate ~f:(fun r -> "lendingRate", [Float.to_string r]);
 *       (match tif with
 *        | Some `Fill_or_kill -> Some ("fillOrKill", ["1"])
 *        | Some `Immediate_or_cancel -> Some ("immediateOrCancel", ["1"])
 *        | _ -> None);
 *       (if post_only then Some ("postOnly", ["1"]) else None)
 *     ] in
 *   safe_post ?buf ~key ~secret ~data trading_url >>|
 *   Result.bind ~f:(Yojson_encoding.destruct_safe (or_error OrderResponse.encoding)) *)

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

module OpenOrder = struct
  module T = struct
    type t = {
      id: int ;
      ts: Time_ns.t ;
      side: Side.t ;
      price: float ;
      starting_qty: float ;
      qty: float ;
      margin: int ;
    } [@@deriving sexp]

    let create ~id ~ts ~side ~price ~starting_qty ~qty ~margin =
      { id ; ts ; side ; price ; starting_qty ; qty ; margin }

    let compare t t' = Int.compare t.id t'.id
  end
  include T
  module Set = Set.Make(T)

  let encoding =
    let open Json_encoding in
    conv
      (fun _ -> assert false)
      (fun (orderNumber, typ, rate, startingAmount, amount, _total, date, margin) ->
         let id = Int.of_string orderNumber in
         let side = Side.of_string typ in
         let price = Float.of_string rate in
         let qty = Float.of_string amount in
         let starting_qty = Float.of_string startingAmount in
         let ts = Time_ns.of_string (date ^ "Z") in
         let margin = margin in
         { id ; ts ; side ; price ; starting_qty ; qty ; margin })
      (obj8
         (req "orderNumber" string)
         (req "type" string)
         (req "rate" string)
         (req "startingAmount" string)
         (req "amount" string)
         (req "total" string)
         (req "date" string)
         (req "margin" int))
end

(* let open_orders ?buf ?(symbol="all") ~key ~secret () =
 *   let data = [
 *     "command", ["returnOpenOrders"];
 *     "currencyPair", [symbol];
 *   ] in
 *   let map_f = Yojson_encoding.destruct_safe OpenOrder.encoding in
 *   safe_post ?buf ~key ~secret ~data trading_url >>| Result.bind ~f:begin function
 *     | `List oos ->
 *       Result.return [symbol, List.map oos ~f:map_f]
 *     | `Assoc oo_assoc -> begin
 *         try
 *           Result.return @@
 *           List.Assoc.map oo_assoc ~f:begin function
 *             | `List oos -> List.map oos ~f:map_f
 *             | #Yojson.Safe.t -> raise Exit
 *           end
 *         with exn -> Http_error.data_encoding exn
 *       end
 *     | #Yojson.Safe.t -> Http_error.data_shape "expected object"
 *   end *)

module TradeHistory = struct
  type trade_category =
    | Exchange
    | Margin
    | Settlement [@@deriving sexp]

  let trade_category_of_string = function
    | "exchange" -> Exchange
    | "marginTrade" -> Margin
    | "settlement" -> Settlement
    | s -> invalid_argf "trade_category_of_string: %s" s ()

  module T = struct
    type t = {
      gid: int;
      id: int;
      ts: Time_ns.t;
      price: float ;
      qty: float ;
      fee: float ;
      order_id: int;
      side: Side.t;
      category: trade_category
    } [@@deriving sexp]

    let compare t t' = Int.compare t.id t'.id
  end
  include T
  module Set = Set.Make(T)

  let encoding =
    let open Json_encoding in
    conv
      (fun _ -> failwith "unsupported")
      (fun (globalTradeID, tradeID, date, rate, amount,
            _total, fee, orderNumber, typ, category) ->
        let id = Int.of_string tradeID in
        let ts = Time_ns.of_string @@ date ^ "Z" in
        let price = Float.of_string rate in
        let qty = Float.of_string amount in
        let fee = Float.of_string fee in
        let order_id = Int.of_string orderNumber in
        let side = Side.of_string typ in
        let category = trade_category_of_string category in
        { gid = globalTradeID ; id ; ts ; price ; qty ; fee ; order_id ; side ; category })
      (obj10
         (req "globalTradeID" int)
         (req "tradeID" string)
         (req "date" string)
         (req "rate" string)
         (req "amount" string)
         (req "total" string)
         (req "fee" string)
         (req "orderNumber" string)
         (req "type" string)
         (req "category" string))
end

(* let trade_history ?buf ?(symbol="all") ?start ?stop ~key ~secret () =
 *   let data = List.filter_opt [
 *       Some ("command", ["returnTradeHistory"]);
 *       Some ("currencyPair", [symbol]);
 *       Option.map start ~f:begin fun ts ->
 *         "start", [Int63.to_string (int63_of_ts ts)]
 *       end ;
 *       Option.map stop ~f:begin fun ts ->
 *         "end", [Int63.to_string (int63_of_ts ts)]
 *       end ;
 *     ] in
 *   let map_f = Yojson_encoding.destruct_safe TradeHistory.encoding in
 *   safe_post ?buf ~key ~secret ~data trading_url >>| Result.bind ~f:begin function
 *     | `List ths -> Result.return @@ [symbol, List.map ths ~f:map_f]
 *     | `Assoc oo_assoc ->
 *       begin
 *         try
 *           Result.return @@ List.Assoc.map oo_assoc ~f:begin function
 *             | `List oos -> List.map oos ~f:map_f
 *             | #Yojson.Safe.t -> raise Exit
 *           end
 *         with exn -> Http_error.data_encoding exn
 *       end
 *     | #Yojson.Safe.t -> Http_error.data_shape "expected object"
 *   end *)

module MarginPosition = struct
  type t = {
    price: float ;
    qty: float ;
    total: float ;
    pl: float ;
    lending_fees: float ;
    side: Side.t ;
  } [@@deriving sexp]

  type side = Long | Short | Flat

  let side_encoding =
    let open Json_encoding in
    string_enum [
      "long", Long ;
      "short", Short ;
      "none", Flat ;
    ]

  let encoding =
    let open Json_encoding in
    let open Encoding in
    conv
      (fun _ -> assert false)
      (fun ((), (price, qty, total, pl, lending_fees, side)) ->
         match side with
         | Flat -> None
         | Long ->
           Some { price ; qty ; total ; pl ; lending_fees ; side = Buy }
         | Short ->
           Some { price ; qty ; total ; pl ; lending_fees ; side = Sell })
      (merge_objs unit
         (obj6
            (req "basePrice" polo_fl)
            (req "amount" polo_fl)
            (req "total" polo_fl)
            (req "pl" polo_fl)
            (req "lendingFees" polo_fl)
            (req "type" side_encoding)))
end

(* let margin_positions ?buf ~key ~secret () =
 *   let data = ["command", ["getMarginPosition"]; "currencyPair", ["all"]] in
 *   let destruct = Yojson_encoding.destruct_safe MarginPosition.position_encoding in
 *   safe_post ?buf ~key ~secret ~data trading_url >>| Result.bind ~f:begin function
 *     | `Assoc ps_assoc ->
 *       Ok (List.map ps_assoc ~f:(fun (symbol, p) -> symbol, destruct p))
 *     | #Yojson.Safe.t -> Http_error.data_shape "expected object"
 *   end *)
