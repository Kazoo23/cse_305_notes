(*insert x in to sorted list xs *)

let rec insert (x : int) (xs: int list) : int list = 
  match xs with
  | [] -> [x]   (*iterates over each element in list xs*)
  | hd :: tl ->
    if hd < x then (*if head element is less than the current element it inserts x after it*)
      hd :: insert x tl
    else
      x :: xs  (*if x is less then the head of the list it is inserted before the head element*)
type il = int list



let rec insert_sort(xs : il) : il =

  let rec aux (sorted : il) (unsorted : il) : il =
    match unsorted with
    | [] -> sorted  (*unsorted and sorted are the same*)
    | hd :: tl -> aux (insert hd sorted) tl (*iterates every element in unsorted then runs the insert function to sort the element into the sorted list*)
  in 
  aux [] xs