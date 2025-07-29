module Qt = Lib.Quadtree_str
module Rand = Lib.Random_gen
module Graph = Lib.Graphics_qt
module Phys = Lib.Physics_qt

let fun1 () =
  Graphics.open_graph "";
  ignore @@ Graphics.wait_next_event [Key_pressed];
  Unix.sleepf 1.5;
  Graphics.close_graph ();
  Unix.sleepf 1.5;
  Graphics.open_graph "";
  ignore @@ Graphics.wait_next_event [Key_pressed];
  Unix.sleepf 1.5;
  Graphics.close_graph ()
  
let () = 
  fun1 ()
