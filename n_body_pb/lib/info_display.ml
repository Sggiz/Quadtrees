let print_line i s =
  Format.printf "| %d : %s@," i s

let display_init_info fun_name_list = 
  Format.printf "@[<v>|@,| Quadtree application : N-body problem@,|@,";
  List.iteri print_line fun_name_list;
  Format.printf "|@, -> @]";
  Format.print_flush ()