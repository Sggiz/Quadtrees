module Qt = Lib.Quadtree_str
module Rand = Lib.Random_gen
module Graph = Lib.Graphics_qt
module Phys = Lib.Physics_qt
module Info = Lib.Info_display

exception ExitToMenu

let exit_menu () =
  Graphics.display_mode true;
  Graph.full_clear ();
  ignore @@ raise ExitToMenu


let static_quadtree_ex n =
  let obj_list = Rand.gen_simple_obj_list n in
  let qt = ref (List.fold_left Qt.add_obj Qt.init_qt obj_list) in
  
  let decision (s : Graphics.status) =
    if s.key = 'e' then raise Exit
    else if s.button then begin
      let x, y = Graph.window_to_canvas (s.mouse_x, s.mouse_y) in
      if 0. <= x && x <= 1. && 0. <= y && y <= 1. then (
        qt := Qt.add_obj !qt (1., x, y);
        Graph.display_quadtree !qt;
        Graphics.synchronize ()
      ) end
  in

  Graph.display_quadtree !qt;
  Graphics.display_mode false;
  Graphics.loop_at_exit [Graphics.Button_down; Graphics.Key_pressed] decision;
  exit_menu ()

let static_quadtree n =
  let obj_list = Rand.gen_simple_obj_list n in
  let qt = ref (List.fold_left Qt.add_obj Qt.init_qt obj_list) in

  let rec decision () =
    let s = Graphics.wait_next_event [Graphics.Button_down; Graphics.Key_pressed] in
    if s.key = 'e' then exit_menu ()
    else if s.button then begin
      let x, y = Graph.window_to_canvas (s.mouse_x, s.mouse_y) in
      if 0. <= x && x <= 1. && 0. <= y && y <= 1. then (
        qt := Qt.add_obj !qt (1., x, y);
        Graph.display_quadtree !qt;
        Graphics.synchronize ()
      );
      decision ()
    end
    else decision ()
  in

  Graph.display_quadtree !qt;
  Graphics.display_mode false;
  decision ()


let naive_grav_simulation n =
  let ol = ref @@ Rand.gen_simple_obj_list n
  and vl = ref @@ List.init n (fun _ -> 0.,0.) in

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
    else if status.key = 'e' then exit_menu ()
    else if status.key = ' ' then simul ()

  and simul () =
    if Graphics.key_pressed () then (
      let k = Graphics.read_key () in
      if k = ' ' then constr_pl () else
      if k = 'e' then exit_menu ()
    ) else
  
    let up_ol, up_vl = Phys.update_state_naive !ol !ol !vl in
    ol := up_ol; vl := up_vl;
    Graph.clear_canvas ();
    List.iter Graph.draw_point !ol;
    Graphics.synchronize ();
    ignore (Unix.select [] [] [] Phys.dt);
    simul ()
  in
  
  List.iter Graph.draw_point !ol;
  Graphics.display_mode false;
  constr_pl ()


let qt_grav_simulation n =
  let ol = ref @@ Rand.gen_simple_obj_list n
  and vl = ref @@ List.init n (fun _ -> 0.,0.) in

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
    else if status.key = 'e' then exit_menu ()
    else if status.key = ' ' then simul ()

  and simul () =
    if Graphics.key_pressed () then (
      let k = Graphics.read_key () in
      if k = ' ' then constr_pl () else
      if k = 'e' then exit_menu ()
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
  
  List.iter Graph.draw_point !ol;
  Graphics.display_mode false;
  constr_pl ()




let () = 
  let function_list = [
    static_quadtree;
    naive_grav_simulation;
    qt_grav_simulation
  ] in
  let function_name_list = [
    "Static quadtree representation";
    "Naive n-body problem simulation";
    "N-body problem simulation using the quadtree structure"
  ] in

  let rec poll_simtype () =
    let s = Info.input_sim_type function_name_list in
    if s = "e" || s = "exit" then (Graphics.close_graph (); -1) else
    let i = int_of_string s in
    if 0 <= i && i <= (List.length function_list) - 1 then i
    (*  let n = Info.input_n () in
      Graph.init_canvas ();
      (List.nth function_list  i) n
    *)
    else poll_simtype ()
  in

  let rec decision () =
    try begin
      let i = poll_simtype () in
      if i = -1 then () else (
        let n = Info.input_n () in
        Graph.clear_canvas ();
        (List.nth function_list i) n
      )
    end
    with ExitToMenu -> decision ()
  in

  Graph.init_canvas ();
  decision ()