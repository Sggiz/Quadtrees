type obj = float * float * float  (* masse, x, y*)
type space_pos = float * float * float (* (midx, midy,sidelength) *)

type quadtree =
    | Void of space_pos
    | Point of obj * space_pos
    | Node of obj * space_pos * quadtree * quadtree * quadtree * quadtree

let init_qt = Void((0.5, 0.5, 1.))

let get_direction (x_ref, y_ref) (x, y) =
    (* Direction repr par:
            2 | 3
            0 | 1
    *)
    (if x <= x_ref then 0 else 1) + (if y <= y_ref then 0 else 2)

let rec add_obj (qt : quadtree) ((m,x,y) : obj) =
    match qt with
    |Void(sp) -> Point((m,x,y), sp)
    
    |Point((m1,x1,y1), (xref, yref, l)) ->
        let d = l/.4. in
        let sp0 = (xref -. d, yref -. d, l/.2.)
        and sp1 = (xref +. d, yref -. d, l/.2.)
        and sp2 = (xref -. d, yref +. d, l/.2.)
        and sp3 = (xref +. d, yref +. d, l/.2.) in
        
        let qt1 = (
            match (get_direction (xref, yref) (x1,y1)) with
            |0 -> Node((0.,0.,0.), (xref, yref, l),
                        Point((m1,x1,y1), (sp0)), Void(sp1), Void(sp2), Void(sp3))
            |1 -> Node((0.,0.,0.), (xref, yref, l),
                        Void(sp0), Point((m1,x1,y1), (sp1)), Void(sp2), Void(sp3))
            |2 -> Node((0.,0.,0.), (xref, yref, l),
                        Void(sp0), Void(sp1), Point((m1,x1,y1), (sp2)), Void(sp3))
            |3 -> Node((0.,0.,0.), (xref, yref, l),
                        Void(sp0), Void(sp1), Void(sp2), Point((m1,x1,y1), (sp3)))
            |_->Void((0.,0.,0.))
        ) in
        add_obj qt1 (m,x,y)
    
    |Node(_, (xref,yref,l), qt0, qt1, qt2, qt3) ->
        match (get_direction (xref, yref) (x,y)) with
        |0 -> Node((0.,0.,0.), (xref,yref,l), (add_obj qt0 (m,x,y)), qt1, qt2, qt3)
        |1 -> Node((0.,0.,0.), (xref,yref,l), qt0, (add_obj qt1 (m,x,y)), qt2, qt3)
        |2 -> Node((0.,0.,0.), (xref,yref,l), qt0, qt1, (add_obj qt2 (m,x,y)), qt3)
        |3 -> Node((0.,0.,0.), (xref,yref,l), qt0, qt1, qt2, (add_obj qt3 (m,x,y)))
        |_->Void((0.,0.,0.))