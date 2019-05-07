(* standard binary heap *)
(* real implementation would use arrays from SML, but we are avoiding that *)
datatype 'a heap = EmptyHeap | Heap of 'a * 'a heap * 'a heap

(* would be better to cache this inside each node *)
(* see "heap2.sml" for how that could be done *)
fun minheight EmptyHeap = 0
  | minheight (Heap (_,left,right)) = Int.min(minheight left,minheight right)+1;

fun maxheight EmptyHeap = 0
  | maxheight (Heap (_,left,right)) = Int.max(maxheight left,maxheight right)+1;

fun insert _ EmptyHeap v = Heap (v,EmptyHeap,EmptyHeap)
  | insert cmp (Heap (head,left,right)) v = 
     if minheight left > minheight right then
       let
         val Heap (subhead,subleft,subright) = insert cmp right v
       in
         if cmp (subhead,head) then
           Heap (subhead,left,Heap (head,subleft,subright))
         else
           Heap (head,left,Heap (subhead,subleft,subright))
       end
     else
       let
         val Heap (subhead,subleft,subright) = insert cmp left v
       in
         if cmp (subhead,head) then
           Heap (subhead,Heap (head,subleft,subright),right)
         else
           Heap (head,Heap (subhead,subleft,subright),right)
       end;

fun extract _ EmptyHeap = raise Empty
  | extract cmp (Heap (head,left,right)) =
     let
       fun removelast EmptyHeap = raise Empty
         | removelast (Heap (head,EmptyHeap,EmptyHeap)) = (head,EmptyHeap)
         | removelast (Heap (head,left,right)) =
          if maxheight left > maxheight right then
            let
              val (retval,newleft) = removelast left
            in
              (retval,Heap(head,newleft,right))
            end
          else
            let 
              val (retval,newright) = removelast right
            in
              (retval,Heap(head,left,newright))
            end

       fun heapify EmptyHeap = EmptyHeap
         | heapify (Heap (head,EmptyHeap,EmptyHeap)) =
            Heap(head,EmptyHeap,EmptyHeap)
         | heapify (Heap (head,Heap(subhead,subleft,subright),EmptyHeap)) =
            if cmp (head,subhead) then
              Heap(head,Heap(subhead,subleft,subright),EmptyHeap)
            else
              Heap(subhead,heapify (Heap(head,subleft,subright)),EmptyHeap)
         | heapify (Heap (head,EmptyHeap,Heap(subhead,subleft,subright))) =
               (* shouldn't happen, but just for completeness *)
            if cmp (head,subhead) then
              Heap(head,EmptyHeap,Heap(subhead,subleft,subright))
            else
              Heap(subhead,EmptyHeap,heapify (Heap(head,subleft,subright)))
         | heapify (Heap (head,Heap(lefthead,leftleft,leftright),
                               Heap(righthead,rightleft,rightright))) =
            if cmp (lefthead,head) then
              if cmp (lefthead,righthead) then
                Heap(lefthead,heapify (Heap(head,leftleft,leftright)),
                              Heap(righthead,rightleft,rightright))
              else
                Heap(righthead,Heap(lefthead,leftleft,leftright),
                              heapify (Heap(head,rightleft,rightright)))
            else
              if cmp (righthead,head) then
                Heap(righthead,Heap(lefthead,leftleft,leftright),
                              heapify (Heap(head,rightleft,rightright)))
              else
                Heap(head,Heap(lefthead,leftleft,leftright),
                          Heap(righthead,rightleft,rightright))

       fun replacehead v EmptyHeap = EmptyHeap
         | replacehead v (Heap(_,left,right)) = Heap(v,left,right)

       val (newhead,smallerheap) = removelast (Heap (head,left,right))
       val newheap = heapify (replacehead newhead smallerheap)
     in
       (head,newheap)
     end;

fun printheap h =
let
  fun printheaphelp _ EmptyHeap = print ""
    | printheaphelp stem (Heap(head,left,right)) =
    let
      val newstem = stem ^ "  ";
      val _ = printheaphelp newstem left
      val _ = print (stem^(Int.toString head)^"\n")
    in
      printheaphelp newstem right
    end;
in
  printheaphelp "" h
end;


(* here's some code that tests out the heap *)
val insertI = insert (op <);
val extractI = extract (op <);

val t1 = insertI (insertI (insertI (insertI (insertI (insertI (insertI EmptyHeap
4) 2) 0) 5) 8) 1) ~1;

val _ = printheap t1;

val (a1,tr1) = extractI t1;
val _ = printheap tr1;
val (a2,tr2) = extractI tr1;
val _ = printheap tr2;
val (a3,tr3) = extractI tr2;
val _ = printheap tr3;
val (a4,tr4) = extractI tr3;
val _ = printheap tr4;
val (a5,tr5) = extractI tr4;
val _ = printheap tr5;
val (a6,tr6) = extractI tr5;
val _ = printheap tr6;
val (a7,tr7) = extractI tr6;
val _ = printheap tr7;
