/* 
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
*/

// We have to buy at a previous idex/time
// so if we switch the 11 and the 5 from the example above
// then the lowest price will be 5 but the last next price in
// time will be 9 so we can't sell for 11 because that price
// was in the past
// var stockPricesYesterday = [10, 7, 11, 8, 5, 9]
var stockPricesYesterday = [10, 7, 5, 8, 11, 9]

// The brute force approach would be to try every pair of times 
// (treating the earlier time as the buy time and the later time as the sell time) 
// and see which one is higher.

function getMaxProfit1(prices){

   var maxProfit = 0;

   // loop thru every time
   for (var outerTime = 0; outerTime < prices.length; outerTime++) {
      
      console.log("**** outer price = " + prices[outerTime]);

      // for every time/index, go thru every OTHER time
      for (var innerTime = 0; innerTime < prices.length; innerTime++) {

         // for each pair, find the later and the earlier time 
         var earlierTime = Math.min(outerTime, innerTime);
         var laterTime = Math.max(outerTime, innerTime);

         // use the times/index to find the prices
         var earlierPrice = prices[earlierTime];
         var laterPrice = prices[laterTime];

         // see what the profit would be if we bought at the min price
         // and sold at the current price
         console.log("laterPrice - earlierPrice");
         console.log(laterPrice + "-" + earlierPrice);

         var potentialProfit = laterPrice - earlierPrice;

         console.log("potential profit = " + potentialProfit);

         // update max profit if we can do better
         maxProfit = Math.max(maxProfit, potentialProfit);
         console.log("max profit = " + maxProfit);
      }      
   }
}

// But that will take O(n^2) time, since we have two nested loops—for every time, 
// we're going through every other time. Also, it's not correct: we won't ever 
// report a negative profit! Can we do better?


function getMaxProfit2(prices){
   
   var maxProfit = 0;

   // loop thru every time
   for (var earlierTime = 0; earlierTime < prices.length; earlierTime++) {
      
      console.log("**** outer price = " + prices[earlierTime]);
      var earlierPrice = prices[earlierTime];

      // for every time/index, go thru every OTHER time
      for (var laterTime = earlierTime + 1; laterTime < prices.length; laterTime++) {

         var laterPrice = prices[laterTime];

         // see what the profit would be if we bought at the earlier price and sold at the current/later price
         console.log(laterPrice + "-" + earlierPrice);

         var potentialProfit = laterPrice - earlierPrice;

         console.log("potential profit = " + potentialProfit);

         // update max profit if we can do better
         maxProfit = Math.max(maxProfit, potentialProfit);
         console.log("max profit = " + maxProfit);
      }      
   }
}

// *** Greedy Approach ***
// Keep a running maxProfit and start it at $0
// At each iteration find a new maxProfit
// - keep track of the lowest price so far
// - see if we can get a better profit at the current price
function getMaxProfit3(prices){
                  // sell     // buy
   var maxProfit = prices[1] - prices[0];
   var lowestSoFar = prices[0];
   
   // loop thru every time
   for (var i = 1; i < prices.length; i++) {
      // keep track of the lowest price so far
      console.log("current lowestSoFar = " + lowestSoFar);
      console.log("current maxProfit = " + maxProfit);
      
      var profit = prices[i] - lowestSoFar;
      
      // see if we can get a better profit at the current price
      // To make sure we’re always buying at an earlier price, never the currentPrice, let’s 
      // switch the order around so we calculate maxProfit before we update minPrice.
      if(profit > maxProfit){
         maxProfit = profit;
      }
      if(prices[i] < lowestSoFar) {
         lowestSoFar = prices[i];
      }
      console.log("lowestSoFar = " + lowestSoFar);
      console.log("maxProfit = " + maxProfit);
   }
}

function getMaxProfit(prices) {
   
       // make sure we have at least 2 prices
       if (prices.length < 2) {
           throw new Error('Getting a profit requires at least 2 prices');
       }
   
       // we'll greedily update minPrice and maxProfit, so we initialize
       // them to the first price and the first possible profit
       var minPrice = prices[0];
       var maxProfit = prices[1] - prices[0];
   
       // start at the second (index 1) time
       // we can't sell at the first time, since we must buy first,
       // and we can't buy and sell at the same time!
       // if we started at index 0, we'd try to buy *and* sell at time 0.
       // this would give a profit of 0, which is a problem if our
       // maxProfit is supposed to be *negative*--we'd return 0.
       for (var i = 1; i < prices.length; i++) {
           var currentPrice = prices[i];
   
           // see what our profit would be if we bought at the
           // min price and sold at the current price
           var potentialProfit = currentPrice - minPrice;
   
           // update maxProfit if we can do better
           maxProfit = Math.max(maxProfit, potentialProfit);
   
           // update minPrice so it's always
           // the lowest price we've seen so far
           minPrice = Math.min(minPrice, currentPrice);
       }
   
       console.log(maxProfit);
      //  return maxProfit;
   }

stockPricesYesterday = [10, 7, 11, 8, 5, 9]        // profit should = 4
// stockPricesYesterday = [150, 150, 150, 150, 150]   // will return undefined
// stockPricesYesterday = [10, 9, 8, 7, 6, 5]           // will return 0 but we can't break even if the price falls all day
module.exports = getMaxProfit3(stockPricesYesterday);