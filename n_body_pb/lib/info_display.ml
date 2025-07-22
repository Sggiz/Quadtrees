let print_line i s =
  Format.printf "| %d : %s@," i s

let input_sim_type fun_name_list = 
  Format.printf "@[<v>|@,| Quadtree application : N-body problem@,|@,";
  List.iteri print_line fun_name_list;
  Format.printf "|@,| e : exit program@,";
  Format.printf "|@, -> @]";
  Format.print_flush ();
  read_line ()

let input_n () =
  Format.printf "@[<v>| Input initial number of objects :@, -> @]";
  Format.print_flush();
  int_of_string @@ read_line ()