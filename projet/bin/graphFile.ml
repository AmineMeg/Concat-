open FileTreatment;;
open Graph;;


(** Convertit une string en tableau de caractères *)
let explode str =
  let rec exp a b =
    if a < 0 then b
    else exp (a - 1) (str.[a] :: b)
  in
  exp (String.length str - 1) []

(** Retourne une liste du nombre de noeud correspondant à la 
    longueur du résultat attendu *)
let rec nodes_of len ret =
    match len with
    | 0 -> ret
    | _ -> nodes_of (len - 1) ((Node (len - 1)) :: ret)

let labels_of_string expected input nodes =
...



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