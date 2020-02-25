let rec foo (a:int) (b:int) (c:int) (d:int) (e:int) (f:int) : unit =
  print_int a;
  print_int b;
  print_int c;
  print_int d;
  print_int e;
  print_int f in
let rec bar (a:int) (b:int) (c:int) (d:int) (e:int) (f:int) : unit =
  foo b a d e f c in
bar 1 2 3 4 5 6
