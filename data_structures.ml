type my_bool = Tru | Fal 
(*
syntax goes type, then name of the type.
 Everything after = is a constructor
 -constructors must start with a capital
 -each constructor is seperated with or
   *)
type color = Blue | Yellow | Green | Red
(* no limit on the number of constructors*)
let b1 : my_bool = Tru
(*syntax is, let then var name, then :, then type name, then = , then value *)
let b2 : my_bool = Fal
let c1: color = Yellow
let c2 : color = Red

let print_color (c: color) : unit = 
  match c with
  | Blue -> print_string "blue"
  | Yellow -> print_string "yellow" (*created values work like normal values*)
  | Green -> print_string "green"
  | Red -> print_string "red"

type point = float * float
type simple_shape = 
  Circle of point * float (*Reads as a circle which contains a point and a float*)
  | Square of point * float (*floar it circle is radius, float in square is size*)

let origin : point = (0.0,0.0)

let circl1 : simple_shape = Circle (origin,1.0) (*creates a circle with a radius of 1 at 0,0*)
let circl2 : simple_shape = Circle ((1.0,1.0),1.0) (*creates a circle with a radius of 1 at 1,1*)
let square : simple_shape = Square (origin,1.0) (*creates a square at 0,0 with a length of 1*)

(*
   more complex shapes can be defined
   type pooint = float * float
   type radius = float
   type side = float

   type shape = 
   Square of side
   | Ellipse of radius * radius
   |RtTriangle of side * side
   | Polygon of point list
*)

type key = string
type value = int

type tree =
Leaf
| Node of key * value * tree * tree

let rec insert (t:tree) (k:key) (v:value) :tree =
  match t with
  | Leaf -> Node (k,v,Leaf,Leaf)
  | Node (k',v',left,right) ->
    if k < k' then
      Node (k',v',insert left k v, right)
    else if k > k' then
      Node (k',v',left,insert right k v)
    else
      Node (k,v,left,right)