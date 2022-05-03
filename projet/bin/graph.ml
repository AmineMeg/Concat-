open AST;;

exception Empty

module Graph =
struct 
    type label = expression
    type node = Node of int
    type vert = node * node * label list
    type graph = node list * vert list

    (** Affiche les pos_exp *)
    let print_pos_exp exp =
        match exp with
        | Forward n -> print_string "Forward "; print_int n
        | Backward n -> print_string "Backward "; print_int n

    (** Affiche les labels d'une arête *)
    let rec print_labs labs =
        match labs with
        | [] -> print_string " "
        | l :: lt ->
            begin
                match l with
                | Const exp -> 
                    print_string "Const \"";
                    print_string exp;
                    print_string "\"";
                    if lt <> [] 
                    then print_string "; ";
                    print_labs lt
                | Extract (p1, p2) ->
                    print_string "Extract (";
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
        | [] -> print_string "[END GRAPH]\n\n"
        | (Node n1, Node n2, labs) :: t ->
            print_int n1; 
            print_string " -> "; 
            print_int n2;
            print_string " : ";
            print_labs labs;
            print_string "\n";
            print_verts t

    (** TODO : supprimer *)
    let print_node n = 
        let rec aux n =
            match n with 
            | [] -> print_string "[FIN]\n\n"
            | h :: t ->
                begin match h with
                | Node h -> 
                    print_string "Node ";
                    print_int h;
                    print_char ' ';
                    aux t
                end
        in
        print_string "[DEBUT NODE] ";
        aux n

    (** Affiche un graphe *)
    let print (graph : graph) =
        print_string "[BEGIN GRAPH]\n";
        match graph with
        | _, verts ->
            print_verts verts

    (** Intersection de deux set de la forme (i, j) où i et j sont
    les indexes de début et de fin de la fonction extract *)
    let inter_set v1 v2: pos_expression list * pos_expression list * label list =
        let rec vert_in_set v vs =
            match vs with
            | [] -> None
            | h :: t -> 
                if v = h 
                then Some v
                else vert_in_set v t
        in 
        let rec aux v1 v2 acc =
            match v1 with
            | [] -> acc
            | h :: t -> 
                begin
                match vert_in_set h v2 with
                | None -> aux t v2 acc
                | Some l ->
                    aux t v2 (l :: acc)
                end
        in
        match v1, v2 with
        | (p1, p2, c1), (p3, p4, c2) ->
            (aux p1 p3 [], aux p2 p4 [], aux c1 c2 [])
    
    (** Récupère deux ensembles de tous les i et tous les j 
    possibles et renvoie les labels Extract (i, j)  *)
    let get_verts v : label list =
        (*let rec pair_i i js acc : label list =
            match js with
            | [] -> acc
            | j :: js -> 
                pair_i i js (Extract (i, j) :: acc)
        in*)
        let rec pair_i_j is js acc : label list =
            match is, js with
            | _, [] -> 
                acc
            | [], _ -> 
                acc
            | i :: is, j :: js -> 
                pair_i_j is js (Extract (i, j) :: acc)
        in
        match v with
        | (is, js, cs) -> 
            pair_i_j is js cs

    (** Récupère les labels Extract (i, j) et renvoie deux ensembles
    de tous les i et tous les j possibles *)
    let get_set v =
        let rec aux v set_i set_j const=
            match v with
            | [] -> (set_i, set_j, const)
            | Extract (i, j) :: t ->
                begin 
                match List.exists (fun x -> i = x) set_i, 
                List.exists (fun x -> j = x) set_j with
                | true, true -> aux t set_i set_j const
                | true, false -> aux t set_i (j :: set_j) const 
                | false, true -> aux t (i :: set_i) set_j const
                | false, false -> aux t (i :: set_i) (j :: set_j) const
                end
            | h  :: t -> aux t set_i set_j (h :: const)
        in
        aux v [] [] []

    (** Intersection de deux ensembles d'arêtes *)
    let inter_vert v1s v2s nodes len : vert list =
        let rec inter_v_vs v1 v2s nodes len acc : vert list =
            match v1, v2s with
            | _, [] -> 
                acc
            | (Node i1, Node i2, lab1), (Node i3, Node i4, lab2) :: v2s ->
                inter_v_vs v1 v2s nodes len
                ((List.nth  nodes (i1 * len + i3),
                List.nth nodes (i2 * len + i4),
                get_verts (inter_set (get_set lab1) (get_set lab2))) :: acc)
        in 
        let rec inter_vs_vs v1s v2s nodes len acc =
            match v1s with
            | [] -> acc
            | v1 :: v1s ->
                inter_vs_vs v1s v2s nodes len ((inter_v_vs v1 v2s nodes len []) @ acc)
        in
        inter_vs_vs v1s v2s nodes len []

    (** Intersection de deux ensembles de noeuds *)
    let inter_node n1s n2s =
        let rec inter_n_ns i1 n2s len acc =
            match n2s with
            | [] -> acc
            | Node i2 :: n2s ->
                inter_n_ns i1 n2s len (Node (i1 * len + i2) :: acc)
        in
        let rec inter_ns_ns n1s n2s acc =
            match n1s with
            | [] -> acc
            | Node i1 :: n1s ->
                inter_ns_ns n1s n2s (inter_n_ns i1 n2s (List.length n2s) [] @ acc)
        in
        print_node (List.rev (inter_ns_ns n1s n2s []));
        List.rev (inter_ns_ns n1s n2s [])

    (** Intersection de deux graphes *)
    let inter (g1:graph) (g2:graph) : graph = 
        match g1 with
        | n1, v1 ->
            begin 
            match g2 with
            | n2, v2 ->
                let nodes =
                    inter_node n1 n2
                in
                print g1;
                print g2;
                (nodes, inter_vert v1 v2 nodes (List.length n2))
            end 

    (** Crée une liste avec le nombre de noeuds passé en paramètre + 1 *)
    let rec create_node nb acc =
        match nb with
        | 0 -> Node 0 :: acc
        | _ -> create_node (nb - 1) (Node nb :: acc)

    
end