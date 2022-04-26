exception Empty

module type GRAPHTYPE = sig
    type label 
    type node 
    type vert
    type graph 

    val inter : graph -> graph -> graph
end 

module Graph : GRAPHTYPE =
struct 
    type label = string
    type node = Node of int
    type vert = node * node * label list
    type graph = 
        node list * vert list

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
end

open FileTreatment;;
open AST;;


(** Convertit une string en tableau de caractères *)
let explode str =
  let rec exp a b =
    if a < 0 then b
    else exp (a - 1) (str.[a] :: b)
  in
  exp (String.length str - 1) []

(** Enlève le dernier élément d'une liste *)
let rec remove_last l =
    match l with 
    | [] -> raise Not_found
    | [x] -> []
    | h :: t -> h :: (remove_last t)

(** Donne tous les substrings possibles d'une chaîne de caractères et les
    retourne sous la forme d'un index de départ, un index de fin et un 
    tableau de caractères *)
let sub exp =
    let rec sub_of len exp acc =
        match exp with
        | [] -> List.rev acc
        | c :: word -> 
            if len <= (List.length exp) && len > 0
            then sub_of (len - 1) word (c :: acc)
            else List.rev acc
    in
    let rec dim_exp exp acc len strt =
        match exp with
        | [] -> acc
        | c :: word -> 
            if len <= (List.length exp) && len > 0
            then dim_exp word ((strt, (strt + len), 
                (sub_of len exp [])) :: acc) len (strt + 1)
            else dim_exp word acc len (strt + 1)
    in
    let rec all_sub exp len acc =
        if len > List.length exp 
        then acc
        else all_sub exp (len + 1) ((dim_exp exp [] len 0) @ acc) 
    in
    all_sub exp 1 []

(** Crée les labels const depuis les substrings *)
let lab_from_sub sub nodes =
    let rec string_from_tab tab =
        match tab with
        | [] -> ""
        | c :: word -> c^(string_from_tab word)
    in
    match sub with
    | n1, n2, tab -> 
        (List.nth nodes n1, List.nth nodes n2, [Const (string_from_tab tab)])


let rec create_graph_from_line line =
    match line with
    | (input, expect) ->
        let nodes = node_of (String.length expected) []
        in
        (nodes, labels_of_stirng (explode expected) (explode input) nodes)

(** Crée un graphe résultant de l'intersection des graphes
    des différentes lignes du fichier *)
let rec create_graph_from_list file_list =
    match file_list with
    | [] -> ([], [])
    | h :: t ->
        inter create_graph_from_line (create_graph_from_list t)