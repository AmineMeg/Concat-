open Graph;;
open AST;;
open FileTreatment;;


(** Convertit une string en tableau de caractères *)
let explode str =
  let rec exp a b =
    if a < 0 then b
    else exp (a - 1) (str.[a] :: b)
  in
  exp (String.length str - 1) []

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
        | _ :: word -> 
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
let rec lab_from_sub sub nodes =
    let rec string_from_tab tab =
        match tab with
        | [] -> ""
        | c :: word -> String.make 1 c ^ (string_from_tab word)
    in
    match sub with
    | [] -> []
    | (n1, n2, tab) :: t -> 
        if List.length nodes > n1 && List.length nodes > n2
       then
        (List.nth nodes n1, List.nth nodes n2, [Const (string_from_tab tab)]) ::
        (lab_from_sub t nodes)
        else raise Empty

(** Crée le graphe correspondant à une paire d'entrée/sortie *)
let create_graph_from_line line : Graph.graph =
    match line with
    | (_, exp) ->
        let nodes = Graph.create_node ((String.length exp) + 1) []
        in
        (nodes, lab_from_sub (sub (explode exp)) nodes)

(** Crée un graphe résultant de l'intersection des graphes
    des différentes lignes du fichier *)
let rec create_graph_from_list file_list =
    match file_list with
    | [] -> ([], [])
    | h :: t ->
        Graph.inter (create_graph_from_line h) (create_graph_from_list t)

let create_graph_from file =
    create_graph_from_list (file_to_list file)

    