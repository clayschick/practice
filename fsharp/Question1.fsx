
(* 
   Suppose we could access yesterday's stock prices as a list, where:

   The indices are the time in minutes past trade opening time, which was 9:30am local time.
   The values are the price in dollars of Apple stock at that time.
   So if the stock cost $500 at 10:30am, stock_prices_yesterday[60] = 500.

   Write an efficient function that takes stock_prices_yesterday and returns the best profit 
   I could have made from 1 purchase and 1 sale of 1 Apple stock yesterday.

   Example:
     stock_prices_yesterday = [10, 7, 5, 8, 11, 9]

      get_max_profit(stock_prices_yesterday)
      # returns 6 (buying for $5 and selling for $11)
 *)

// let stockPricesYesterday = [10; 7; 11; 8; 5; 9]
let stockPricesYesterday = [10; 9; 8; 7; 6; 5; 4]

// This won't work because it does not consider
// the chronological order of the prices
// we have to buy before we sell
let sortAll xs =
   let rec sort xs l h =
      match xs with
      | x::xs ->
         if x < l then sort xs x h
         else if x > h then sort xs l x
         else sort xs l h
      | [] -> h - l
   sort xs xs.Head 0

let minPrice = sortAll stockPricesYesterday

let getMaxProfit xs =
   let rec sort xs (minPrice : int) (maxProfit : int) =
      match xs with
      | [] -> maxProfit 
      | x::xs ->
            // if xs.Length = 0 then maxProfit else sort xs minPrice maxProfit
            let potentialProfit = x - minPrice
            let newMaxProfit = if potentialProfit > maxProfit then potentialProfit else maxProfit
            let newMinPrice = if xs.Head < minPrice then xs.Head else minPrice
            // The Head of xs has been taken off the seq (x::xs) and it is now empty and will be passed back
            // into the sort function at which point it will be matched as [] (empty) and return maxProfit.
            // So using xs.Head won't work here because there is no Head now.
            sort xs newMinPrice newMaxProfit       
   sort xs xs.[0] ((-) xs.[1]  xs.[0])

let maxProfit = getMaxProfit stockPricesYesterday


let tester = [1 .. 10]

let folderBack elem acc = 
  printfn "the elem = %A" elem
  printfn "the acc = %A" acc
  elem::acc
  
let copyList list = List.foldBack folderBack list []
printfn "%A" (copyList tester)


let folder acc elem = 
  printfn "the elem = %A" elem
  printfn "the acc = %A" acc
  elem::acc

let copyList2 list = List.fold folder [] list
printfn "%A" (copyList2 tester)



let sumListBack list = List.foldBack (fun elem acc -> acc + elem) list 0