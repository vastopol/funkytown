use "tree23.sml";

Control.Print.printDepth := 100;

(* ----- Tree Data ----- *)


val garbagetree =
Node2 ( 10,
        Node2 ( 5,
                Node2 (3, EmptyTree, EmptyTree),
                Node2 (7, EmptyTree, EmptyTree)
              ),
        Node3 ( 20,
                30,
                Node2 (15, EmptyTree, EmptyTree),
                Node2 (25, EmptyTree, EmptyTree),
                Node2 (35, EmptyTree, EmptyTree)
              )
      );


(* ----- find23 ----- *)

find23 intcmp garbagetree 3;
find23 intcmp garbagetree 5;
find23 intcmp garbagetree 7;
find23 intcmp garbagetree 10;
find23 intcmp garbagetree 15;
find23 intcmp garbagetree 20;
find23 intcmp garbagetree 25;
find23 intcmp garbagetree 30;
find23 intcmp garbagetree 35;
find23 intcmp garbagetree 0;
find23 intcmp garbagetree 100;

(* ----- insert23 ----- *)

(* test simple split root *)
val x = EmptyTree;
val x = insert23 intcmp x 10;
val x = insert23 intcmp x 20;
val a = insert23 intcmp x 5;
val b = insert23 intcmp x 15;
val c = insert23 intcmp x 25;

(* test bubble up *)
val x = EmptyTree;
val x = insert23 intcmp x 30;
val x = insert23 intcmp x 70;
val x = insert23 intcmp x 50;
val x = insert23 intcmp x 10;
val x = insert23 intcmp x 90;
val a = insert23 intcmp x 80; (* bubble up right *)
val b = insert23 intcmp x 20; (* bubble up left *)

val x = EmptyTree;
val x = insert23 intcmp x 3;
val x = insert23 intcmp x 5;
val x = insert23 intcmp x 7;
val x = insert23 intcmp x 10;
val a = insert23 intcmp x 8;

val x = EmptyTree;
val x = insert23 intcmp x 5;
val x = insert23 intcmp x 8;
val x = insert23 intcmp x 10;
val x = insert23 intcmp x 20;
val x = insert23 intcmp x 6;
val a = insert23 intcmp x 30;
val b = insert23 intcmp a 4;


