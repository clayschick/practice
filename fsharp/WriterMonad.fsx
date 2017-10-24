(* 

   https://hackernoon.com/a-monad-writer-for-f-26aa987e4a3a

   let writer = bind 5 "starting"

   We need a generic type ‘a and the generic ‘L which is our log. 
   Of course, we are not constrained to log only string. The elements 
   of the log can be, in fact, of any type. For convenience and 
   simplicity, we are going to use string most of the time, but 
   we could use any other type as part of the log.

 *)

type Writer<'a,'L> = AWriter of 'a * 'L

// bind is a function that receives two args, a generic value v and 
// the first item to be logged, itemLog and then it returns our initial Writer.

let bind = function | (v, itemLog) -> AWriter(v, [itemLog])

(* 

   Now, we need a way to map over the value of the Writer, so we 
   can change types of the boxed value without affecting the log. 

   map takes another function fx that is applied over the Writer 
   value a by creating a new Writer. Notice we never mutate a 
   Writer, we just create new Writers.

   ('a -> 'b) -> Writer<'a,c'> -> Writer<'b,'c>

 *)

let map fx = function | AWriter(a, log) -> AWriter(fx a, log)

// ^^^ is the same as:
let map2 fx writer =
   match writer with
   | AWriter(a, log) -> AWriter(fx a, log)

(* 
  
   These are the equivilent:
      function pattern-rules -> expression
      (fun x -> match x with pattern-rules -> expression)
 *)


(* 

   With only these bind and map, we can start defining other functions 
   that return Writers (doing logging) while keeping referential transparency.

   - given the same input parameters, always produces the same output (return value).

 *)

// return a Writer through the function bind
let sum x y = bind(x + y, "summin' some shit") 

let str a = a.ToString()

let to_string a = a |> map str

sum 6 7 |> to_string

sum 4 5 |> map str