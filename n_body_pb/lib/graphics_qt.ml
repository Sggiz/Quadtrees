type obj = Quadtree_str.obj
type space_pos = Quadtree_str.space_pos
type quadtree = Quadtree_str.quadtree

let master_bg_color = Graphics.rgb 43 72 101
let bg_color = Graphics.rgb 20 30 40
let div_color = Graphics.rgb 97 161 225
let point_color = Graphics.rgb 198 227 255

let branch_color = Graphics.rgb 30 40 50
let node_color = Graphics.rgb 100 115 125

let hl_main_color = Graphics.rgb 0 200 0
let hl_point_color = Graphics.rgb 200 0 0
let hl_branch_color = Graphics.rgb 150 30 30

let int_size_x, int_size_y = 800, 800
let ext_size_x, ext_size_y = 850, 850
let upper_margin = (
  if Sys.win32 then 40
  else 0
)
let correction_margin = (
  if Sys.win32 then 15
  else 0
)
let corner_x, corner_y = 
    (ext_size_x - int_size_x)/2 ,
    (ext_size_y - int_size_y)/2

let line_width = 1
let point_radius = 2

let canvas_to_window (x,y) =
    let fx = x *. float int_size_x
    and fy = y *. float int_size_y in
    corner_x + int_of_float fx, corner_y + int_of_float fy

let window_to_canvas (px, py) =
    float (px - corner_x)/. float int_size_x,
    float (py - corner_y) /. float int_size_y

let draw_div ((x,y,l) : space_pos) =
    Graphics.set_color div_color;
    let pvx, pvy = canvas_to_window (x, y -. l/.2.)
    and phx, phy = canvas_to_window (x -. l/.2., y)
    and dx = int_of_float (l *. float int_size_x)
    and dy = int_of_float (l *. float int_size_y) in
    Graphics.moveto pvx pvy;
    Graphics.rlineto 0 dy;
    Graphics.moveto phx phy;
    Graphics.rlineto dx 0

let draw_point ((_, x, y) : obj) =
    Graphics.set_color point_color;
    let px, py = canvas_to_window (x,y) in
    Graphics.fill_circle px py point_radius

let rec draw_explore (qt:quadtree) =
    match qt with
    |Void(_) -> ()
    |Point(o,_) -> draw_point o
    |Node(_, sp, qt0, qt1, qt2, qt3) ->
        draw_div sp;
        draw_explore qt0; draw_explore qt1; draw_explore qt2; draw_explore qt3

let rec draw_branching (qt:quadtree) ((_,x,y):obj) =
    let ix, iy = canvas_to_window (x,y) in
    match qt with
    |Void(_) -> ()
    |Point((m,px,py), _) -> 
        let ox, oy = canvas_to_window (px,py) in
        Graphics.set_color branch_color;
        Graphics.moveto ix iy;
        Graphics.lineto ox oy;
        draw_point (m,px,py)
    |Node((m,px,py),_,qt0,qt1,qt2,qt3) ->
        let ox, oy = canvas_to_window (px,py) in
        Graphics.set_color branch_color;
        Graphics.moveto ix iy;
        Graphics.lineto ox oy;
        Graphics.set_color node_color;
        Graphics.fill_circle ox oy point_radius;
        let a = [|qt0;qt1;qt2;qt3|] in
        for i = 0 to 3 do
            draw_branching a.(i) (m,px,py)
        done

let draw_highlight_calculation qt theta o =
    let rec draw_sub (qt:quadtree) (ix, iy) =
        match qt with
        |Void(_) -> ()
        |Point(o,_) ->
            let _,x,y = o in
            let jx, jy = canvas_to_window (x,y) in
            Graphics.moveto ix iy;
            Graphics.set_color hl_branch_color;
            Graphics.lineto jx jy;
            draw_point o
        |Node(_,_,qt0,qt1,qt2,qt3) ->
            draw_sub qt0 (ix,iy);
            draw_sub qt1 (ix,iy);
            draw_sub qt2 (ix,iy);
            draw_sub qt3 (ix,iy)
    in
    let rec explore (qt:quadtree) theta o =
        let _,x,y = o in
        let ix, iy = canvas_to_window (x,y) in
        match qt with
        |Void(_) -> ()
        |Point((_,px,py),_) ->
            let jx, jy = canvas_to_window (px, py) in
            Graphics.moveto ix iy;
            Graphics.set_color hl_branch_color;
            Graphics.lineto jx jy;
            Graphics.set_color hl_point_color;
            Graphics.fill_circle jx jy point_radius
        |Node((_,px,py),(_,_,l),_,_,_,_) when l /. (sqrt ((px-.x)**2. +. (py-.y)**2.)) < theta ->
            let jx, jy = canvas_to_window (px, py) in
            Graphics.moveto ix iy;
            Graphics.set_color hl_branch_color;
            Graphics.lineto jx jy;
            draw_sub qt (jx, jy);
            Graphics.set_color hl_point_color;
            Graphics.fill_circle jx jy point_radius
        |Node(_,_,qt0,qt1,qt2,qt3) ->
            let a = [|qt0;qt1;qt2;qt3|] in
            for i = 0 to 3 do
                explore a.(i) theta o
            done
    in 
    explore qt theta o;
    Graphics.set_color hl_main_color;
    let _,x,y = o in
    let ix, iy = canvas_to_window (x,y) in
    Graphics.fill_circle ix iy point_radius

let full_clear () =
    Graphics.set_color master_bg_color;
    Graphics.fill_rect 0 (-1) ext_size_x ext_size_y

let clear_canvas () =
    Graphics.set_color bg_color;
    Graphics.fill_rect 
        (corner_x - 2*point_radius) (corner_y - 2*point_radius)
        (int_size_x + 4*point_radius) (int_size_y + 4*point_radius)

let init_canvas () =
    Graphics.open_graph (Printf.sprintf " %dx%d" (ext_size_x + correction_margin) (ext_size_y + upper_margin));
    Graphics.set_window_title "Quadtree graphic display";
    Graphics.set_line_width line_width;

    full_clear ();
    clear_canvas ()

let display_quadtree qt =
    clear_canvas ();
    draw_explore qt
