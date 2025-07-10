open Lib

(*let init_qt = Quadtree_str.Void((0.5, 0.5, 1.))*)

let print_int i = print_endline (string_of_int i)

let () = 
  for k = 0 to 10 do
    print_int (Random_gen.u k)
  done
  