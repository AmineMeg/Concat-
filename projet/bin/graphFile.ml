open FileTreatment;;
open Graph;;


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

(** Retourne une liste du nombre de noeud correspondant à la 
    longueur du résultat attendu *)
let rec nodes_of len ret =
    match len with
    | 0 -> ret
    | _ -> nodes_of (len - 1) ((Node (len - 1)) :: ret)

(** Donne tous les substrings possibles d'une chaîne de caractères *)
let substring_in expected =
    let rec sub_from_end expected (ret:(string list) list) =
        match expected with
        | [] -> ret
        | c :: word -> 
            sub_from_end word ([c] :: (List.map (fun x -> x @ [c]) ret))
    in
    let rec sub_from_start expected ret =
        match expected with
        | [] -> ret
        | _ -> 
            sub_from_start 
            (remove_last expected) 
            ((sub_from_end expected []) @ ret)
    in
    sub_from_start expected []


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