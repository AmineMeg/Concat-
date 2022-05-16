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

    (** Retourne la liste ns1 sans les éléments de ns2 *)
    let rec minus ns1 ns2 =
        let rec min ns n =
            match ns with
            | [] -> []
            | h :: ns -> 
                if h = n 
                then min ns n 
                else 
                h :: (min ns n)
        in
        match ns2 with
        | [] -> ns1
        | n :: ns2 ->
            minus (min ns1 n) ns2

    (** Renvoie la liste des noeuds atteignables depuis start_node *)
    let reachable_nodes vs start_node =
        let rec reachable_nodes_from vs start_node n =
            match vs with
            | [] -> 
                start_node
            | (n1, n2, labs) :: vs ->
                if n1=n && labs<>[] && (not (List.mem n2 start_node))
                then reachable_nodes_from vs (n2 :: start_node) n
                else reachable_nodes_from vs start_node n
        in
        let rec aux vs visited non_visited acc =
            match non_visited with
            | [] -> acc
            | n :: non_visited ->
                let new_start =
                    reachable_nodes_from vs acc n
                in                 
                aux vs 
                (n :: visited) 
                (non_visited @ (minus (minus new_start non_visited) visited)) 
                (new_start)
        in
        print_node (aux vs [] start_node start_node);
        aux vs [] start_node start_node

    (** Renvoie une liste où tous les noeuds sont à 100 sauf le 
    noeud initial 
    let init_dist nds =
        let rec create_list i nb =
            match nb with
            | 0 -> []
            | _ -> i :: (create_list i (nb - 1))
        in
        let rec init_node dis ns n =
            match dis, ns with
            | d :: dis, Node nd :: nds  when n=nd ->
                0 :: dis
            | d :: dis, Node nd :: nds ->
                d :: (init_node dis nds n)
            | _, _ -> 
                raise Not_found
        in
        init_node (create_list 100 (List.length nds)) nds 0*)

    (** Renvoie le couple (noeud, distance) de la plus petite 
    distance *)
    let smallest ns ds : node =
        let rec aux ns ds min n_min : node =
            match ns, ds with
            | n :: ns, d :: ds ->
                if d < min
                then aux ns ds d n 
                else aux ns ds min n_min
            | [], [] -> n_min
            | _, _ -> raise Not_found
        in
        aux ns ds (-1) (List.hd ns)

    (** Donnne le poids d'une arete *)
    let get_weight_of_label l =
        let get_weight_of_a_single_label _l=
            match _l with 
                |Const(_) -> 1
                |Extract(_) -> 2
        in match l with
        |([]) -> raise (Invalid_argument ("label n'a pas la bonne taille"))
        |(h::[]) -> get_weight_of_a_single_label h
        |(_::_) -> raise (Invalid_argument ("label n'a pas la bonne taille"))


    (** Retourne la distance actuelle du noeud *)
    let rec weight_node n dist nodes=
        match dist, nodes with
        | d1 :: _, n1 :: _ when n=n1 ->
            d1
        | _ :: dist, _ :: nodes -> 
            weight_node n dist nodes
        | _, _ -> raise (Invalid_argument ("Ici on attend des listes de même longueur"))

    (** Met à jour les distances *)
    let rec update_dist nodes dist n w : int list =
        match nodes, dist with
        | n1 :: _, d :: dist when n1=n -> 
            (d + w) :: dist
        | n :: nodes, d :: dist -> 
            d :: (update_dist nodes dist n w)
        | _, _ -> raise (Invalid_argument ("Problèmes node/distance"))

    (** Met à jour les predecesseurs *)
    let rec update_pred nodes pred n2 n1 =
        match nodes, pred with
        | n :: _, _ :: pred when n2=n ->
            n1 :: pred
        | _ :: nodes, _ :: pred ->
            update_pred nodes pred n2 n1
        | _, _ -> raise (Invalid_argument ("Problèmes node/distance"))

    (** Met à jour les distances *)
    let update n nodes (dist: int list) vs pred =
        let rec update_from_to n1 n2 vs dist (pred: node list) =
            match vs with
            | (vs_n1, vs_n2, labs) :: vs ->
                if vs_n1 = n1 && vs_n2 = n2 
                then 
                    if get_weight_of_label labs + weight_node n1 dist nodes < weight_node n2 dist nodes
                    then
                        update_dist nodes dist n2 (get_weight_of_label labs + weight_node n1 dist nodes),
                        update_pred nodes pred n2 n1
                    else dist, pred
                else update_from_to n1 n2 vs dist pred
            | [] -> raise Empty
        in
        let rec update_rec n nodes dis_pre vs  =
            match nodes, dis_pre with
            | [], (dist, pred) -> dist, pred
            | node :: nodes, (dist, pred) -> 
                update_rec n nodes (update_from_to n node vs dist pred) vs 
        in
        match nodes with
        | [] -> raise Empty
        | node :: nodes -> 
            (minus nodes [n]), update_rec n nodes (update_from_to n node vs dist pred) vs 



    let dijkstra g =
        let rec init_dist ns i =
            match ns with
            | [] -> []
            | Node n :: t -> 
                if n = i 
                then 0 :: (init_dist t i)
                else 100 :: (init_dist t i)
        in
        let init ns i =
            (ns, init_dist ns i, ns)
        in
        let aux ns vs trip =
            match trip with
            | (partition, dist, pred) ->
                update (smallest partition dist) ns dist vs pred
        in
        match g with
        | ns, vs ->
            aux ns vs (init ns 0)
            

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

    (** Ne laisse qu'une pos_exp par arête *)
    let clean_graph g =
        let reach ns vs =
            match ns with
            | n :: _ -> reachable_nodes vs [n]
            | [] -> raise Empty 
        in
        let rec clean_verts vs ns =
            match vs with
            | (n1, n2, l :: _) :: vs ->
                if (List.exists (fun x -> x=n1) ns)
                then (n1, n2, [l]) :: (clean_verts vs ns)
                else clean_verts vs ns
            | (_, _, []) :: vs -> 
                clean_verts vs ns
            | [] -> []
        in
        match g with 
        | ns, vs ->
            reach ns vs, clean_verts vs (reach ns vs)

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
                clean_graph (nodes, inter_vert v1 v2 nodes (List.length n2))
            end 

    (** Crée une liste avec le nombre de noeuds passé en paramètre + 1 *)
    let rec create_node nb acc =
        match nb with
        | 0 -> Node 0 :: acc
        | _ -> create_node (nb - 1) (Node nb :: acc)

    
end