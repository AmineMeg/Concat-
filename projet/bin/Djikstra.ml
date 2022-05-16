open Set
open Array
type vertex = int
type weight = float
type neighbor = vertex * weight

module VertexSet = Set.Make(
  struct type t = weight * vertex 
  let compare = compare 
  end
)

let rec create_adj g =
  match g with 
  |(n, v)-> 
    begin 
      match v with 
      | (n1, n2, label)-> 
    
    end

let adj =


let disjkstra src adj = 
  let n = Array.length adj in
    let dis = Array.make n infinity in
      dis.(src) <- 0.0;
  let previous = Array.make n (-1) in 
  let rec aux vertex_queue = 
    if not (VertexSet.is_empty vertex_queue) then
      let d, u = VertexSet.min_elt vertex_queue in
      let vertex_queue' = VertexSet.remove (d, u) vertex_queue in
      let edges = adj.(u) in
      let f vertex_queue (v, weight) =
        if d +. weight >= dis.(v) then vertex_queue
        else begin
          let vertex_queue' = VertexSet.remove (dis.(v), v) vertex_queue in
          dis.(v) <- (d +. weight);
          previous.(v) <- u;
          VertexSet.add (dis.(v), v) vertex_queue'
        end
      in 
       aux (List.fold_left f vertex_queue' edges)
      in
      aux (VertexSet.singleton(dis.(src),src));
      dis,previous

let print_path endd previous =
  let rec find_path endd ls = 
    if endd = -1 then ls
    else find_path previous.(endd) (endd :: ls)
  in find_path endd []


let () = 
  let dis, previous = disjkstra 0 adj in
  let n = Array.length dis in
    begin
      for i = 1 to n - 1 do 
        Printf.printf "Distance 0 to %d : %f\tand the path is: " i dis.(i);
        let path_list = print_path i previous in
        let path_string = List.map string_of_int path_list in
        let res = String.concat "->" path_string in
            Printf.printf "%s\n" res;
      done;
    end;