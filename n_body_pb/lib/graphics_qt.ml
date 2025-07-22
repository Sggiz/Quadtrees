type obj = Quadtree_str.obj
type space_pos = Quadtree_str.space_pos
type quadtree = Quadtree_str.quadtree

let master_bg_color = Graphics.rgb 43 72 101
let bg_color = Graphics.rgb 20 30 40
let div_color = Graphics.rgb 97 161 225
let point_color = Graphics.rgb 198 227 255

let int_size_x, int_size_y = 600, 600
let ext_size_x, ext_size_y = 650, 650
let upper_margin = 40
let correction_margin = 15
let corner_x, corner_y = 
    (ext_size_x - int_size_x)/2 ,
    (ext_size_y - int_size_y)/2

let line_width = 3
let point_radius = 1

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

let full_clear () =
    Graphics.set_color master_bg_color;
    Graphics.fill_rect 0 (-1) ext_size_x ext_size_y

let clear_canvas () =
    Graphics.set_color bg_color;
    Graphics.fill_rect 
        (corner_x - 2*point_radius) (corner_y - 2*point_radius)
        (int_size_x + 4*point_radius) (int_size_y + 4*point_radius)

let init_canvas () =
    Graphics.open_graph (Printf.sprintf "%dx%d" (ext_size_x + correction_margin) (ext_size_y + upper_margin));
    Graphics.set_window_title "Quadtree graphic display";
    Graphics.set_line_width line_width;

    full_clear ();
    clear_canvas ()

let display_quadtree qt =
    clear_canvas ();
    draw_explore qt