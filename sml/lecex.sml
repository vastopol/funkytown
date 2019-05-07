fun polyeval [] _ = 0.0
  | polyeval (c::cs) x = c+x*(polyeval cs x);

fun polyeval2 coeffs x =
let
  fun accsumandpow (c,(s,p)) = (s+c*p,x*p)
  val (ans,_) = foldl accsumandpow (0.0,1.0) coeffs
in
  ans
end;


datatype color = red | green | blue;

fun isred x = (x=red);

fun tostring red = "red"
  | tostring blue = "blue"
  | tostring green = "green";

datatype number = whole of int | decimal of real | NaN;

val x = whole 2;
(* x+x will not work *)
val (whole y) = x;  (* now y is the integer used to create x *)
val ydoubled = y+y;



datatype intlist = EmptyIntList | IntCons of int*intlist;

val longintlist = IntCons(1,IntCons(5,IntCons(0,IntCons(~1,EmptyIntList))));


datatype inttree = EmptyIntTree | IntNode of int*inttree*inttree;

fun addtointtree EmptyIntTree v = IntNode(v,EmptyIntTree,EmptyIntTree)
  | addtointtree (IntNode(rootval,left,right)) v =
         if rootval<v then IntNode(rootval,addtointtree left v,right)
                else IntNode(rootval,left,addtointtree right v);

datatype 'a tree = EmptyTree | Node of 'a * 'a tree * 'a tree;

fun addtotree _ EmptyTree v = Node(v,EmptyTree,EmptyTree)
  | addtotree cmp (Node(rootval,left,right)) v =
         if cmp (v,rootval) then Node(rootval,addtotree cmp left v,right)
                else Node(rootval,left,addtotree cmp right v);

fun isintree _ EmptyTree _ = false
  | isintree cmp (Node(rootval,left,right)) v =
         if cmp (rootval,v) then isintree cmp right v
         else if cmp(v,rootval) then isintree cmp left v
         else true;

fun printtree t =
let
  fun printhelper _ EmptyTree = print ""
    | printhelper stem (Node(v,left,right)) =
    let
      val newstem = stem ^ "  "
      val _ = printhelper newstem left
      val _ = print (stem ^ (Int.toString v) ^ "\n")
    in
      printhelper newstem right
    end
in
  printhelper "" t
end;

val addtoItree = addtotree (op <);

val t1 = addtoItree EmptyTree 1;
val t2 = addtoItree t1 4;
val t3 = addtoItree t2 2;
val t4 = addtoItree t3 ~1;
val t5 = addtoItree t4 ~10;
val t6 = addtoItree t5 5;
val t7 = addtoItree t6 ~5;


val _ = printtree t7;

val arethey = map (isintree (op <) t7) [~2,~1,0,1,2,3,4,5,6];

