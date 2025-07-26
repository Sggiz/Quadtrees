type obj = Quadtree_str.obj
type space_pos = Quadtree_str.space_pos
type quadtree = Quadtree_str.quadtree
let is_obj_in_node = Quadtree_str.is_obj_in_node

let g = 0.0001 (*constante de gravitation*)
let theta = 2. /. 3.
let dt = 1./.180.
let d_lim = 0.01

let dist (_,x1,y1) (_,x2,y2) = sqrt ((x2-.x1)**2. +. (y2-.y1)**2.)

let update_v (vx,vy) (ax, ay) =
    (vx +. ax *. dt, vy +. ay *. dt)

let update_obj (m,x,y) (vx ,vy) =
    (m, x +. vx*.dt, y +. vy*.dt)

let compute_acc o ast =
    let d = dist o ast in
    if d < d_lim then (0., 0.) else
    let (_,x,y), (ma,xa,ya) = o, ast in
    let const = g *. ma /. (dist o ast ** 3.) in
    (const *. (xa -. x), const *. (ya -. y))


let rec compute_acc_tot_naive (ol: obj list) (o: obj) =
    match ol with
    |[] -> (0., 0.)
    |ast::q ->
        let (ax0,ay0) = compute_acc_tot_naive q o
        and (ax1,ay1) = compute_acc o ast in
        (ax1 +. ax0, ay1 +. ay0)

let rec update_state_naive oltot ol vl =
    match ol, vl with
    |o::oq, v::vq -> 
        let ol0, vl0 = update_state_naive oltot oq vq
        and vx,vy = update_v v (compute_acc_tot_naive oltot o) in
        let m,x,y = update_obj o (vx,vy) in
        if x < 0. then (m,0.,y)::ol0, (-.vx, vy)::vl0 else
        if x > 1. then (m,1.,y)::ol0, (-.vx, vy)::vl0 else
        if y < 0. then (m,x,0.)::ol0, (vx, -.vy)::vl0 else
        if y > 1. then (m,x,1.)::ol0, (vx, -.vy)::vl0 else
        (m,x,y)::ol0, (vx,vy)::vl0
    |_ -> [], []


let rec compute_acc_tot (qt: quadtree) (o: obj) =
    match qt with
    |Void(_) -> (0., 0.)
    |Point(ast,_) when ast = o -> (0.,0.)
    |Point(ast,_) -> compute_acc o ast
    |Node(cm, (_,_,l),_,_,_,_) when not (is_obj_in_node o qt) && (l /. dist o cm) < theta -> compute_acc o cm
    |Node(_,_,qt0,qt1,qt2,qt3) ->
        let (fx0,fy0), (fx1,fy1), (fx2,fy2), (fx3,fy3) = 
        compute_acc_tot qt0 o, compute_acc_tot qt1 o, compute_acc_tot qt2 o, compute_acc_tot qt3 o in
        (fx0 +. fx1 +. fx2 +. fx3, fy0 +. fy1 +. fy2 +. fy3)

let rec update_state qt ol vl =
    match ol, vl with
    |[], [] -> [], []
    |o::oq, v::vq -> 
        let ol0, vl0 = update_state qt oq vq
        and vx,vy = update_v v (compute_acc_tot qt o) in
        let m,x,y = update_obj o (vx,vy) in
        if x < 0. then (m,0.,y)::ol0, (-.vx, vy)::vl0 else
        if x > 1. then (m,1.,y)::ol0, (-.vx, vy)::vl0 else
        if y < 0. then (m,x,0.)::ol0, (vx, -.vy)::vl0 else
        if y > 1. then (m,x,1.)::ol0, (vx, -.vy)::vl0 else
        (m,x,y)::ol0, (vx,vy)::vl0
    |_ -> [], []

