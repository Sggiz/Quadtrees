let u0 = 42 

let rec pow2 k =
  if k=0 then 1 else 2* pow2 (k-1)

let cap_nb = pow2 30 (* resolution de 2**30 sur 2**30 *)

let next (u:int) = (1_103_515_245 * uprev + 12_345) mod cap_nb

let rec u (k:int) =
  if k = 0 then u0 else
  let uprev = u (k-1) in
  next uprev

let int_to_segment (i:int) = float i /. float cap_nb

let gen_simple_obj_list (n:int) =
  let rec parcours (k:int) (ux:int) =
    if k = 0 then [] else
    let uy = next ux in
    (1., int_to_segment ux, int_to_segment uy) :: parcours (k-1) (next uy)
  in
  parcours n u0