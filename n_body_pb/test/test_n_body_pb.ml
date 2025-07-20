module Qt = Lib.Quadtree_str
module Rand = Lib.Random_gen
module Graph = Lib.Graphics_qt
module Phys = Lib.Physics_qt

let fun1 () =
  Graph.init_canvas ();
  let rec poll_print () =
    let status = Graphics.wait_next_event [Key_pressed;Button_down] in
    if status.button then () 
    else (
      Printf.printf "%c" status.key;
      poll_print()
    )
  in
  poll_print ();
  Graphics.close_graph ()

let () = 
  fun1 ()
  