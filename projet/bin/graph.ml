module type GRAPHTYPE = sig
    type label = string
    type node = int
    type vert = node * node * label list
    type graph = 
        node list * vert list

    val inter : graph -> graph -> graph
end 

module Graph : GRAPHTYPE =
struct 
    (** Intersection de deux graphes *)
    let inter g1 g2 = 
        match g1 with
        | n1, v1 ->
            begin 
            match g2 with
            | n2, v2 ->
                (inter_node n1 n2, inter_node v1 v2)
            end 

    (** Intersection de deux ensembles de noeuds *)
    let inter_node n1 n2 =
        let rec aux n1 n2 acc =
            match n1 with
            | [] -> n2
            | h :: t -> 
                aux t n2 ((List.map (fun x -> h*10+x) n2) :: acc)
        in
        aux n1 n2 []

    (** Intersection de deux ensembles d'arêtes *)
    let inter_vert v1 v2 =
        let rec aux v1 v2 acc =
            match v1 with 
            | [] -> []
            | h :: t -> 
                inter_verts h v2

        in

    (** Intersection d'une arête et d'un ensemble d'arêtes *)
    let inter_verts v vs =
        match vs with
        | [] -> raise Not_Found
        | h :: t ->
            begin
                match h, v with
                | (n1, n2, lab1), (n1, n2, lab2) ->
                    (n1, n2, inter_lab lab1 lab2)
                | _, _ ->
                    inter_verts v t
            end
    (** Intersection de deux listes de label *)
    let rec inter_lab lab1 lab2 =
        match lab1 with
        | [] -> []
        | h1 :: t1 -> 
            if lab2 = [] 
            then []
            else
                (inter_labs h1 lab2) :: (inter_lab t1 lab2)

    (** Intersection d'un label et d'un ensemble de labels *)
    let rec inter_labs lab labs =
        match labs with
        | [] -> raise Empty
        | h :: t -> 
            if lab == h
            then lab
            else inter_labs lab t



end