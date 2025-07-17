module Qt = Lib.Quadtree_str
module Rand = Lib.Random_gen
module Graph = Lib.Graphics_qt
module Phys = Lib.Physics_qt

let static_quadtree n =
  Graph.init_canvas ();

  let obj_list = Rand.gen_simple_obj_list n in
  let qt = List.fold_left Qt.add_obj Qt.init_qt obj_list in

  Graph.display_quadtree qt;
  let _ = Graphics.wait_next_event [Graphics.Key_pressed] in ()

let dynamic_quadtree () =
  let qt = ref Qt.init_qt in
  
  let decision (s : Graphics.status) =
    if not s.button then raise Exit
    else 
      let x, y = Graph.window_to_canvas (s.mouse_x, s.mouse_y) in
      if 0. <= x && x <= 1. && 0. <= y && y <= 1. then (
        qt := Qt.add_obj !qt (1., x, y);
        Graph.display_quadtree !qt
      )
  in

  Graph.init_canvas ();
  Graphics.loop_at_exit [Graphics.Button_down; Graphics.Key_pressed] decision

let manu_naive_grav_simulation () =
  let ol = ref []
  and vl = ref [] in

  let rec constr_pl () =
    let status = Graphics.wait_next_event [Button_down; Key_pressed] in

    if status.button then
      let x, y = Graph.window_to_canvas (status.mouse_x, status.mouse_y) in
      if 0. <= x && x <= 1. && 0. <= y && y <= 1. then (
        ol := (1.,x,y) :: !ol;
        vl := (0.,0.) :: !vl;
        Graph.draw_point (1.,x,y)
      );
      constr_pl ()
    else if status.key = 'e' then raise Exit
    else if status.key = ' ' then simul ()

  and simul () =
    if Graphics.key_pressed () then (
      let k = Graphics.read_key () in
      if k = ' ' then constr_pl () else
      if k = 'e' then raise Exit
    ) else
  
    let up_ol, up_vl = Phys.update_state_naive !ol !ol !vl in
    ol := up_ol; vl := up_vl;
    Graph.clear_canvas ();
    List.iter Graph.draw_point !ol;
    let _ = Unix.select [] [] [] Phys.dt in
    simul ()
  in
  
  Graph.init_canvas ();
  constr_pl ()

let auto_naive_grav_simulation n =
  let ol = ref (Rand.gen_simple_obj_list n)
  and vl = ref (List.init n (fun _ -> (0.,0.))) in

  let rec simul () =
    if Graphics.key_pressed () && Graphics.read_key () = 'e' then raise Exit
    else
    let up_ol, up_vl = Phys.update_state_naive !ol !ol !vl in
    ol := up_ol; vl := up_vl;
    Graph.clear_canvas ();
    List.iter Graph.draw_point !ol;
    let _ = Unix.select [] [] [] Phys.dt in
    simul ()
  in

  Graph.init_canvas ();
  simul ()


let () = 
  manu_naive_grav_simulation ()