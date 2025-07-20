let print_line i s =
  Format.printf "%d : %s@." i s

let display_init_info fun_name_list = 
  List.iteri print_line fun_name_list