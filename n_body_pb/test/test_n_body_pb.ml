let () = 
  print_endline "Hello, World!";
  Graphics.open_graph "";
  let status = Graphics.wait_next_event [Key_pressed] in
  print_endline ("key pressed :" ^ String.make 1 status.key ^"!");
  raise Exit