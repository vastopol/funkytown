(*
1.
reverse a list with out @
use foldl
*)

fun revlist l =
    foldl op:: [] l;


(*
2.
stack
*)

datatype 'a stack = the_stack of 'a list;

fun stack_push ( the_stack(s) ) e =  the_stack( e :: s );

fun stack_pop ( the_stack([]) ) = the_stack([])
|   stack_pop ( the_stack(x::xs) )  = the_stack(xs);

fun stack_top ( the_stack([]) ) = NONE
|   stack_top ( the_stack(x::xs) ) = SOME x;


(*
3.
heap

insert, extract, reorder, print
*)

datatype 'a heap = empty_heap | the_heap of 'a * 'a  heap * 'a heap;

fun minheight empty_heap = 0
|   minheight (the_heap(_,lheap,rheap)) =  1 + Int.min( (minheight lheap) , (minheight rheap) );

fun maxheight empty_heap = 0
|   maxheight (the_heap(_,lheap,rheap)) =  1 + Int.max( (maxheight lheap) , (minheight rheap) );

fun heap_push ( empty_heap ) v =  the_heap( v, empty_heap, empty_heap )
  | heap_push ( the_heap(rootval,left,right) ) v =
         if (minheight left) < (minheight right) then
            let
                val the_heap(v, the_heap(lval,lheap,rheap), r) = the_heap( rootval, (heap_push left v) ,right )
            in
                if lval < v then
                    the_heap(lval, the_heap(v,lheap,rheap), r)
                else
                    the_heap(v, the_heap(lval,lheap,rheap), r)
            end
         else
            let
                val the_heap(v, l, the_heap(rval,lheap,rheap)) = the_heap( rootval, left , (heap_push right v) )
            in
                if rval < v then
                    the_heap(rval, l, the_heap(v,lheap,rheap))
                else
                    the_heap(v, l, the_heap(rval,lheap,rheap))
            end;



fun heap_pop ( empty_heap ) = raise empty_heap
|   heap_pop ( the_heap(rootval,left,right) )  =
        let
            fun get_last empty_heap = raise empty_heap
            |  get_last the_heap(rootval,empty_heap,empthy_heap) = (rootval,empty_heap)
            |   get_last the_heap(rootval,left,right) =
                if (maxheight left) < (maxheight right) then
                    let
                        val (tup1,tup2) = get_last right
                    in
                        (tup1, the_heap(rootval,left,tup2))
                    end
                else
                    let
                        val (tup1,tup2) = get_last left
                    in
                        (tup1, the_heap(rootval,tup2,right))
                    end

            fun heapify empty_heap = empty_heap
            |   heapify the_heap(v,empty_heap,empthy_heap) = the_heap(v,empty_heap,empthy_heap)
            |   heapify the_heap(v, the_heap(v1,l1,r1), the_heap(v2,l2,r2)) =
                if v > v1 andalso v1 < v2 then
                    the_heap(v1, (heapify the_heap(v,l1,r1)), the_heap(v2,l2,r2) )
                else if v > v2 andalso v2 < v1 then
                    the_heap(v2, the_heap(v1,l1,r1), (heapify the_heap(v,l2,r2)) )
                else
                     the_heap(v, the_heap(v1,l1,r1), the_heap(v2,l2,r2)) (* probably not going to happen *)

            fun replacehead =


        in

        end
;




