(* 
   You have an array of integers, and for each index you want to find the product of 
   every integer except the integer at that index. Write a function GetProductsOfAllIntsExceptAtIndex() 
   that takes an array of integers and returns an array of the products.

   For example, given:

     [1, 7, 3, 4]

   your function would return:

     [84, 12, 28, 21]

   by calculating:

     [7 * 3 * 4,  1 * 3 * 4,  1 * 7 * 4,  1 * 7 * 3]

   Do not use division in your solution.

 *)

let values = [| 1; 7; 3; 4 |]

let GetProductsOfAllIntsExceptAtIndex (valArr : int[]) =
   let results = Array.create valArr.Length 0

   for i in 0 .. (valArr.Length - 1) do
      let mutable product = 1
      for j in 0 .. (valArr.Length - 1) do
         // j is the value, how can I check the index of j against the value of i?
         if j = i then ()
         else product <- product * valArr.[j]
         
      printfn "Product = %i" product
      results.[i] <- product
   results

let resultArray = GetProductsOfAllIntsExceptAtIndex values

// Accesses elements from 2 to the end of the array.
// array1.[2..]

// let array3 = [| for i in 1 .. 10 -> i * i |] // <- squares all the elements of the array
// let resultEasy = [| for i in valArr do  |]

// printfn "%A" (Array.collect (fun elem -> [| 0 .. elem |]) [| 1; 5; 10|])
// [|0; 1; 0; 1; 2; 3; 4; 5; 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10|]