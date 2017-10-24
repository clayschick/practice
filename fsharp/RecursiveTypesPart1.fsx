type Book = { title: string; price: decimal }

type ChocolateType =
   | Dark
   | Milk
   | SeventyPercent

type Chocolate = { chocType: ChocolateType; price: decimal }

type WrappingPaperStyle =
   | Birthday
   | Holiday
   | SolidColor

type Gift =
 | Book of Book
 | Chocolate of Chocolate
 | Wrapped of Gift * WrappingPaperStyle
 | Boxed of Gift
 | WithACard of Gift * message:string

let wolfHall = { title = "Wolf Hall"; price = 20m }
let delishChoclate = { chocType = Milk;  price = 15m }

let birthdayPresent = WithACard (Wrapped (Book wolfHall, Birthday), "Happy Birthday")

let christmasGift = WithACard (Wrapped (Boxed (Chocolate delishChoclate), Holiday), "Merry Christmas")

(* 
   First, say that we want a description of the gift. The logic will be:

   - For the two non-recursive cases, return a string describing that case.
   - For the three recursive cases, return a string that describes the case, but also
     includes the description of the inner gift. This means that description function
     is going to refer to itself, and therefore it must be marked with the rec keyword.
 *)

let rec description gift =
   match gift with
   | Book book -> sprintf "'%s'" book.title
   | Chocolate choc -> sprintf "%A chocolate" choc.chocType
   | Wrapped (innerGift, paperStyle) -> sprintf "%A wrapped in %A wrapping paper" (description innerGift) paperStyle
   | Boxed innerGift -> sprintf "%A in a box" (description innerGift)
   | WithACard (innerGift, message) -> sprintf "%A with a card saying '%s'" (description innerGift) message

birthdayPresent |> description

christmasGift |> description

let rec totalCost gift =
   match gift with 
   | Book book -> book.price
   | Chocolate choc -> choc.price
   | Wrapped (innerGift,style) -> (totalCost innerGift) + 0.5m
   | Boxed innerGift -> (totalCost innerGift) + 1.0m
   | WithACard (innerGift,message) -> (totalCost innerGift) + 2.0m

birthdayPresent |> totalCost
christmasGift |> totalCost

let rec whatsInside gift =
   match gift with 
   | Book book -> "A book"
   | Chocolate choc -> "Some chocolate"
   | Wrapped (innerGift,style) -> whatsInside innerGift
   | Boxed innerGift -> whatsInside innerGift
   | WithACard (innerGift,message) -> whatsInside innerGift

birthdayPresent |> whatsInside
christmasGift |> whatsInside 

//* That's a lot of duplicate code

(* 
   There is application logic and navigation logic (pattern matching?) in each
   function that needs to be seperated by parameterizing all the things.

   We can parameterize the application logic as functions and pass them in.

   let rec cataGift fBook fChocolate fWrapped fBoxed fWithACard gift =
      match gift with
      | Book book -> fBook book
      | Chocolate choco -> fChocolate choco
      | Wrapped (innerGift, paperStyle) -> 
         let innerGiftResult = cataGift fBook fChocolate fWrapped fBoxed fWithACard innerGift
         fWrapped (innerGiftResult, paperStyle)
      | Boxed innerGift -> 
         let innerGiftResult = cataGift fBook fChocolate fWrapped fBoxed fWithACard innerGift
         fBoxed innerGiftResult
      | WithACard (innerGift, message) -> 
         let innerGiftResult = cataGift fBook fChocolate fWrapped fBoxed fWithACard innerGift
         fWithACard (innerGiftResult, message) 
 
   The cataGift function we wrote above is called a “catamorphism”, from the Greek components “down + shape”. 
   In normal usage, a catamorphism is a function that “collapses” a recursive type into a new
   value based on the recursive type's structure. In fact, you can think of a catamorphism as a sort of “visitor pattern”.

   A catamorphism is very powerful concept, because it is the most fundamental function that you 
   can define for a structure like this. Any other function can be defined in terms of it.

   That is, if we want to create a function with signature Gift -> string or Gift -> int, we can use a 
   catamorphism to create it by specifying a function for each case in the Gift structure.

   Note: Gift -> string 
   -- collapse a Gift recursive type into a new value like int based on the structure of the Gift
      or how it is nested (that's the structure part)

   -------------------------------------------------------------------

   We can tidy up the cata implementation.
   First, the cataGift fBook fChocolate fWrapped fBox fCard crops up three times, once 
   for each recursive case. Let’s assign it a name like recurse:

   let rec cataGift fBook fChocolate fWrapped fBoxed fWithACard gift =
      let recurse = cataGift fBook fChocolate fWrapped fBoxed fWithACard
      match gift with
      | Book book -> fBook book
      | Chocolate choco -> fChocolate choco
      | Wrapped (innerGift, paperStyle) -> 
         let innerGiftResult = recurse innerGift
         fWrapped (innerGiftResult, paperStyle)
      | Boxed innerGift -> 
         let innerGiftResult = recurse innerGift
         fBoxed innerGiftResult
      | WithACard (innerGift, message) -> 
         let innerGiftResult = recurse innerGift
         fWithACard (innerGiftResult, message) 

   The recurse function has the simple signature Gift -> 'a – that is, it converts a Gift
   to the return type we need, and so we can use it to work with the various innerGift values.

   The other thing is to replace innerGift with just gift in the recursive cases – this is 
   called “shadowing”. The benefit is that the “outer” gift is no longer visible to the 
   case-handling code, and so we can’t accidentally recurse into it, which would cause an infinite loop.

   Normally a bad idea but helps prevent bugs in this case 
 *)

let rec cataGift fBook fChocolate fWrapped fBoxed fWithACard gift: 'r =  // <-- naming the return type 'r
      let recurse = cataGift fBook fChocolate fWrapped fBoxed fWithACard
      match gift with
      | Book book -> 
         fBook book
      | Chocolate choco -> 
         fChocolate choco
      | Wrapped (gift, paperStyle) -> 
         fWrapped (recurse gift, paperStyle)
      | Boxed gift -> 
         fBoxed (recurse gift)
      | WithACard (gift, message) -> 
         fWithACard (recurse gift, message)

// Now rewrite the totalcosts function

let totalCostUsingCata gift = 
   let fBook (book:Book) = 
      book.price
   let fChocolate (choco:Chocolate) =
      choco.price
   let fWrapped (innerCost, paperStyle) = 
      innerCost + 0.5m
   let fBoxed innerCost = 
      innerCost + 1.0m
   let fWithACard (innerCost, message) =
      innerCost + 2.0m
   // call the catamorphism passing in the handler functions along with the Gift
   cataGift fBook fChocolate fWrapped fBoxed fWithACard gift

birthdayPresent |> totalCostUsingCata

christmasGift |> totalCostUsingCata

let descriptionUsingCata gift = 
   let fBook (book:Book) = 
      sprintf "'%s'" book.title
   let fChocolate (choco:Chocolate) =
      sprintf "%A chocolate" choco.chocType
   let fWrapped (innerText, paperStyle) = 
      sprintf "%A wrapped in %A wrapping paper" innerText paperStyle
   let fBoxed innerText = 
      sprintf "%A in a box" innerText
   let fWithACard (innerText, message) =
      sprintf "%A with a card saying '%s'"innerText message
   // call the catamorphism passing in the handler functions along with the Gift
   cataGift fBook fChocolate fWrapped fBoxed fWithACard gift

christmasGift |> descriptionUsingCata


(* 
   Rules for creating catamorphisms:

   - Create a function parameter to handle each case in the structure.
   - For non-recursive cases, pass the function parameter all the data associated with that case.
   - For recursive cases, perform two steps:
      -- First, call the catamorphism recursively on the nested value.
      -- Then pass the handler all the data associated with that case, 
         but with the result of the catamorphism replacing the original nested value.
 *)