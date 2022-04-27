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
    let rec in_verts v vs =
        match vs with
        | [] -> None
        | h :: t ->
            begin
                match h, v with
                | (n1, n2, _), (n3, n4, _) when n1 = n3 && n2 = n4 ->
                    Some ()
                | _, _ ->
                    in_verts v t
            end

    (** Intersection de deux ensembles d'arêtes *)
    let inter_vert v1 v2 =
        let rec aux v1 v2 acc =
            match v1 with 
            | [] -> acc
            | h :: t -> 
                match in_verts h v2 with
                | None -> aux t v2 acc
                | _ -> aux t v2 (h :: acc)
        in
        aux v1 v2 []

    (** Intersection de deux ensembles de noeuds *)
    let inter_node n1 n2 =
        let rec aux n1 n2 acc =
            match n1 with
            | [] -> acc
            | h :: t -> 
                match h with
                | Node h ->
                    aux t n2 ((List.map (fun x -> begin match x with Node x -> Node (h*10+x) end) n2) @ acc)
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
        | 0 -> Node 0 :: acc
        | _ -> create_node (nb - 1) (Node nb :: acc)

    (** Affiche les pos_exp *)
    let print_pos_exp exp =
        match exp with
        | Forward n -> print_string "Forward "; print_int n
        | Backward n -> print_string "Backward "; print_int n

    (** Affiche les labels d'une arête *)
    let rec print_labs labs =
        match labs with
        | [] -> print_string "] "
        | l :: lt ->
            begin
                match l with
                | Const exp -> 
                    print_string "Const ";
                    print_string exp;
                    if lt <> [] 
                    then print_string "; ";
                    print_labs lt
                | Extract (p1, p2) ->
                    print_string "(";
                    print_pos_exp p1;
                    print_string ", ";
                    print_pos_exp p2;
                    print_string ")";
                    print_string "; ";
                    print_labs lt
            end
    
    (** Affiche les arêtes d'un graphe *)
    let rec print_verts verts =
        match verts with 
        | [] -> print_string "\n[end graph]\n"
        | (Node n1, Node n2, labs) :: t ->
            print_int n1; 
            print_string " -> "; 
            print_int n2;
            print_string " : [";
            print_labs labs;
            print_string "\n";
            print_verts t

    (** Affiche un graphe *)
    let print (graph : graph) =
        print_string "[begin graph]\n";
        match graph with
        | _, verts ->
            print_verts verts
end