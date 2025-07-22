module Qt = Lib.Quadtree_str
module Rand = Lib.Random_gen
module Graph = Lib.Graphics_qt
module Phys = Lib.Physics_qt
module Info = Lib.Info_display


let static_quadtree n =
  let obj_list = Rand.gen_simple_obj_list n in
  let qt = List.fold_left Qt.add_obj Qt.init_qt obj_list in

  Graph.display_quadtree qt;
  ignore (Graphics.wait_next_event [Graphics.Key_pressed]);
  Graphics.close_graph ()


let dynamic_quadtree () =
  let qt = ref Qt.init_qt in
  
  let decision (s : Graphics.status) =
    if s.key = 'e' then raise Exit
    else if s.button then begin
      let x, y = Graph.window_to_canvas (s.mouse_x, s.mouse_y) in
      if 0. <= x && x <= 1. && 0. <= y && y <= 1. then (
        qt := Qt.add_obj !qt (1., x, y);
        Graph.display_quadtree !qt
      ) end
  in

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
        Graph.draw_point (1.,x,y);
        Graphics.synchronize( )
      );
      constr_pl ()
    else if status.key = 'e' then Graphics.close_graph ()
    else if status.key = ' ' then simul ()

  and simul () =
    if Graphics.key_pressed () then (
      let k = Graphics.read_key () in
      if k = ' ' then constr_pl () else
      if k = 'e' then Graphics.close_graph ()
    ) else
  
    let up_ol, up_vl = Phys.update_state_naive !ol !ol !vl in
    ol := up_ol; vl := up_vl;
    Graph.clear_canvas ();
    List.iter Graph.draw_point !ol;
    Graphics.synchronize ();
    ignore (Unix.select [] [] [] Phys.dt);
    simul ()
  in
  
  Graphics.display_mode false;
  constr_pl ()


let auto_naive_grav_simulation n =
  let ol = ref (Rand.gen_simple_obj_list n)
  and vl = ref (List.init n (fun _ -> (0.,0.))) in

  let rec simul () =
    if Graphics.key_pressed () && Graphics.read_key () = 'e' then Graphics.close_graph ()
    else
    let up_ol, up_vl = Phys.update_state_naive !ol !ol !vl in
    ol := up_ol; vl := up_vl;
    Graph.clear_canvas ();
    List.iter Graph.draw_point !ol;
    Graphics.synchronize ();
    ignore (Unix.select [] [] [] Phys.dt);
    simul ()
  in

  Graphics.display_mode false;
  simul ()


let manu_qt_grav_simulation () =
  let ol = ref []
  and vl = ref [] in

  let rec constr_pl () =
    let status = Graphics.wait_next_event [Button_down; Key_pressed] in

    if status.button then
      let x, y = Graph.window_to_canvas (status.mouse_x, status.mouse_y) in
      if 0. <= x && x <= 1. && 0. <= y && y <= 1. then (
        ol := (1.,x,y) :: !ol;
        vl := (0.,0.) :: !vl;
        Graph.draw_point (1.,x,y);
        Graphics.synchronize ()
      );
      constr_pl ()
    else if status.key = 'e' then Graphics.close_graph ()
    else if status.key = ' ' then simul ()

  and simul () =
    if Graphics.key_pressed () then (
      let k = Graphics.read_key () in
      if k = ' ' then constr_pl () else
      if k = 'e' then Graphics.close_graph ()
    ) else
    
    let qt = Qt.compute_cm (List.fold_left Qt.add_obj Qt.init_qt !ol) in
    let up_ol, up_vl = Phys.update_state qt !ol !vl in
    ol := up_ol; vl := up_vl;
    Graph.clear_canvas ();
    List.iter Graph.draw_point !ol;
    Graphics.synchronize ();
    ignore (Unix.select [] [] [] Phys.dt);
    simul ()
  in
  
  Graphics.display_mode false;
  constr_pl ()


let auto_qt_grav_simulation n =
  let ol = ref (Rand.gen_simple_obj_list n)
  and vl = ref (List.init n (fun _ -> (0.,0.))) in

  let rec simul () =
    if Graphics.key_pressed () && Graphics.read_key () = 'e' then Graphics.close_graph ()
    else
    
    let qt = Qt.compute_cm (List.fold_left Qt.add_obj Qt.init_qt !ol) in
    let up_ol, up_vl = Phys.update_state qt !ol !vl in
    ol := up_ol; vl := up_vl;
    Graph.clear_canvas ();
    List.iter Graph.draw_point !ol;
    Graphics.synchronize ();
    ignore (Unix.select [] [] [] Phys.dt);
    simul ()
  in

  Graphics.display_mode false;
  simul ()




let () = 
  let n = 3000 in
  let function_list = [
    (fun () -> static_quadtree n);
    dynamic_quadtree;
    manu_naive_grav_simulation;
    (fun () -> auto_naive_grav_simulation n);
    manu_qt_grav_simulation;
    (fun () -> auto_qt_grav_simulation n)
  ] in
  let function_name_list = [
    Printf.sprintf "Static quadtree representation of %d random objects" n;
    "Interactive static quadtree representation";
    "Interactive naive n-body problem simulation";
    Printf.sprintf "Naive n-body problem simulation of %d random objects" n;
    "Interactive n-body problem simulation using the quadtree structure";
    Printf.sprintf "N-body problem simulation using the quadtree structure on %d random objects" n
  ] in

  Info.display_init_info function_name_list;

  let rec poll_simtype () =
    let c = (read_line ()).[0] in
    if c = 'e' then Graphics.close_graph () else
    let i = int_of_char c in
    if 0 <= i-48 && i-48 <= (List.length function_list) - 1 then (
      Graph.init_canvas ();
      (List.nth function_list  (i-48)) ()
    )
    else poll_simtype ()
  in
  poll_simtype ()