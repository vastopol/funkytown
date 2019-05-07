(* Program B - 2 3 Tree *)


(* ----- Question 1 ----- *)

(* 2-3 Tree data type *)

datatype 'a baltree = EmptyTree
                    | Node2 of 'a * 'a baltree * 'a baltree
                    | Node3 of 'a * 'a * 'a baltree * 'a baltree * 'a baltree;


(* ----- Question 2 ----- *)

(* 2 3 Tree find function *)

(*  Type:  (’a * ’b -> int) -> ’b baltree -> ’a -> ’b option  *)

fun find23 _ EmptyTree _ =  NONE

|   find23 cmp (Node2(midval,ltree,rtree)) item =
        if (cmp (item,midval)) = 0 then
            SOME midval
        else if (cmp (item, midval)) = ~1 then
            find23 cmp ltree item
        else
            find23 cmp rtree item

|   find23 cmp (Node3(loval,hival,ltree,mtree,rtree)) item =
        if (cmp (item, loval)) = 0 then
            SOME loval
        else if (cmp (item, hival)) = 0 then
            SOME hival
        else if (cmp (item, loval)) = ~1 then
            find23 cmp ltree item
        else if (cmp (item, loval)) = 1 andalso (cmp (item, hival)) = ~1 then
            find23 cmp mtree item
        else
            find23 cmp rtree item ;


(* ----- Question 3 ----- *)

(* 2 3 Tree insert function *)

(*  Type:  ('a * 'a -> int) -> 'a baltree -> 'a -> 'a baltree  *)

datatype 'a my_problem = problem of 'a * 'a baltree * 'a baltree | ok of 'a baltree;

fun insert23 comp tree value =
let

    fun helper (ok(atree)) = atree
    |   helper (problem(extra, lsubtree, rsubtree)) =
            Node2(extra,lsubtree,rsubtree)

    fun my_insert23 _ EmptyTree item = ok(Node2(item, EmptyTree, EmptyTree))    (* emptytree *)

    |   my_insert23 cmp (Node2(midval, EmptyTree, EmptyTree)) item =        (* 2node with no children *)
            if (cmp (item, midval)) = ~1 then
                ok(Node3(item, midval, EmptyTree, EmptyTree, EmptyTree))
            else
                ok(Node3(midval, item, EmptyTree, EmptyTree, EmptyTree))

    |   my_insert23 cmp (Node3(loval, hival, EmptyTree, EmptyTree, EmptyTree)) item =  (* 3node with no children *) (* return problem *)
            if (cmp (item, loval)) = ~1 then
                problem(loval, Node2(item, EmptyTree, EmptyTree), Node2(hival, EmptyTree, EmptyTree))
            else if (cmp (item, loval)) = 1 andalso (cmp (item, hival)) = ~1 then
                problem(item, Node2(loval, EmptyTree, EmptyTree), Node2(hival, EmptyTree, EmptyTree))
            else
                problem(hival, Node2(loval, EmptyTree, EmptyTree), Node2(item, EmptyTree, EmptyTree))

    |   my_insert23 cmp (Node2(midval, ltree, rtree)) item =    (* recurse on a 2 node *)
        let
            val ret = (* find the side to recurse on, get the return value either problem | ok *)
                if (cmp (item, midval)) = ~1 then
                      (my_insert23 cmp ltree item)
                else
                      (my_insert23 cmp rtree item)
        in
            case ret of
                 problem(v, Node2(v1,l1,r1), Node2(v2,l2,r2)) =>
                    if (cmp (item, midval)) = ~1 then
                        ok(Node3(v, midval, Node2(v1,l1,r1), Node2(v2,l2,r2), rtree))
                    else
                        ok(Node3(midval, v, ltree, Node2(v1,l1,r1), Node2(v2,l2,r2)))
                | ok(Node3(v1, v2, l, m, r)) =>
                    if (cmp (item, midval)) = ~1 then
                        ok(Node2(midval, Node3(v1, v2, l, m, r), rtree))
                    else
                        ok(Node2(midval, ltree, Node3(v1, v2, l, m, r)))
        end

    |   my_insert23 cmp (Node3(loval, hival, ltree, mtree, rtree)) item =    (* recurse on a 3 node *)
        let
            val ret = (* find the side to recurse on, get the return value either problem | ok *)
                if (cmp (item, loval)) = ~1 then
                    (my_insert23 cmp ltree item)
                else if (cmp (item, loval)) = 1 andalso (cmp (item, hival)) = ~1 then
                    (my_insert23 cmp mtree item)
                else
                    (my_insert23 cmp rtree item)
        in
            case ret of
                 problem(v, Node2(v1,l1,r1), Node2(v2,l2,r2)) => (* splitting  *)
                    if (cmp (item, loval)) = ~1 then
                        problem(loval,
                                Node2(v, Node2(v1, l1, r1), Node2(v2 ,l2, r2) ),
                                Node2(hival, mtree, rtree ) )
                    else if (cmp (item, loval)) = 1 andalso (cmp (item, hival)) = ~1 then
                        problem(v,
                                Node2(loval, ltree, Node2(v1, l1, r1) ),
                                Node2(hival, Node2(v2 ,l2, r2), rtree ) )
                    else
                        problem(hival,
                                Node2( loval, ltree, mtree),
                                Node2(v, Node2(v1, l1, r1), Node2(v2 ,l2, r2) ) )
                | ok(Node3(v1, v2, l, m, r)) =>
                    if (cmp (item, loval)) = ~1 then
                        ok(Node3(loval, hival, Node3(v1, v2, l, m, r), mtree, rtree))
                    else if (cmp (item, loval)) = 1 andalso (cmp (item, hival)) = ~1 then
                        ok(Node3(loval, hival, ltree, Node3(v1, v2, l, m, r), rtree))
                    else
                        ok(Node3(loval, hival, ltree, mtree, Node3(v1, v2, l, m, r)))
        end

in
    helper (my_insert23 comp tree value)
end;

(* ----- Question 4 ----- *)

(* Helper comparison function for find23 *)
(* first argument = value for the search , second argument = value from the tree *)

fun intcmp (s1:int,s2:int) =
    if s1<s2 then ~1 else if s2<s1 then 1 else 0;

fun charcmp (s1:char,s2:char) =
    if s1<s2 then ~1 else if s2<s1 then 1 else 0;

fun listcmp _ ([],[]) = 0
    | listcmp _ (_,[]) = 1
    | listcmp _ ([],_) = ~1
    | listcmp cmp (a::ta,b::tb) =
    let val c = cmp(a,b) in if c=0 then listcmp cmp (ta,tb) else c end;

