module Qt = Lib.Quadtree_str
module Rand = Lib.Random_gen
module Graph = Lib.Graphics_qt

let n = 100

let () = 
  let obj_list = Rand.gen_simple_obj_list n in

  let qt = List.fold_left Qt.add_obj Qt.init_qt obj_list in
  
  Graph.display_quadtree qt