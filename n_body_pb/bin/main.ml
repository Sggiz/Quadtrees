module Qt = Lib.Quadtree_str
module Rand = Lib.Random_gen
module Graph = Lib.Graphics_qt

let static_quadtree n =
  let obj_list = Rand.gen_simple_obj_list n in

  let qt = List.fold_left Qt.add_obj Qt.init_qt obj_list in
  
  Graph.init_canvas ();
  Graph.display_quadtree qt;
  let _ = Graphics.wait_next_event [Graphics.Key_pressed] in ()

let dynamic_quadtree () =
  let qt = ref Qt.init_qt in
  
  let decision (s : Graphics.status) =
    if s.button then (
      let x, y = Graph.window_to_canvas (s.mouse_x, s.mouse_y) in
      qt := Qt.add_obj !qt (1., x, y);
      Graph.display_quadtree !qt
    )
    else raise Exit
  in

  Graph.init_canvas ();
  Graphics.loop_at_exit [Graphics.Button_down; Graphics.Key_pressed] decision
  

let () = 
  dynamic_quadtree ()