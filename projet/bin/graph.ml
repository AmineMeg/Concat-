open AST;;

exception Empty

module Graph =
struct 
    type label = expression
    type node = Node of int
    type vert = node * node * label list
    type graph = node list * vert list

    (** Intersection d'un label et d'un ensemble de labels *)
    let rec inter_labs lab labs =
        match labs with
        | [] -> raise Empty
        | h :: t -> 
            if lab == h
            then lab
            else inter_labs lab t

    (** Intersection de deux listes de label *)
    let rec inter_lab lab1 lab2 =
        match lab1 with
        | [] -> []
        | h1 :: t1 -> 
            if lab2 = [] 
            then []
            else
                (inter_labs h1 lab2) :: (inter_lab t1 lab2)

    (** Intersection d'une arête et d'un ensemble d'arêtes *)
    let rec inter_verts v vs =
        match vs with
        | [] -> raise Not_found
        | h :: t ->
            begin
                match h, v with
                | (n1, n2, lab1), (n3, n4, lab2) when n1 = n3 && n2 = n4 ->
                    (n1, n2, inter_lab lab1 lab2)
                | _, _ ->
                    inter_verts v t
            end

    (** Intersection de deux ensembles d'arêtes *)
    let inter_vert v1 v2 =
        let rec aux v1 v2 acc =
            match v1 with 
            | [] -> acc
            | h :: t -> 
                aux t v2 ((inter_verts h v2) :: acc)
        in
        aux v1 v2 []

    (** Intersection de deux ensembles de noeuds *)
    let inter_node n1 n2 =
        let rec aux n1 n2 acc =
            match n1 with
            | [] -> n2
            | h :: t -> 
                match h with
                | Node h ->
                    aux t n2 ((List.map (fun x -> begin match x with Node x -> h*10+x end) n2) :: acc)
        in
        aux n1 n2 []

    (** Intersection de deux graphes *)
    let inter (g1:graph) (g2:graph) : graph = 
        match g1 with
        | n1, v1 ->
            begin 
            match g2 with
            | n2, v2 ->
                (inter_node n1 n2, inter_vert v1 v2)
            end 

    (** Crée une liste avec le nombre de noeuds passé en paramètre + 1 *)
    let rec create_node nb acc =
        match nb with
        | 0 -> acc
        | _ -> create_node (nb - 1) (Node nb :: acc)
end