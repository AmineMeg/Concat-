open FileReader;;

(** Convertir un fichier en tableau de string : chaque ligne du
    fichier est une ligne du tableau *)
let from_file_to_list file =
    String.split_on_char '\n' (read_lines file);;

(** Convertit un fichier en tableau de string et affiche chaque
    ligne tu tableau en faisant un retour à la ligne : 
    utilisé pour les test initiaux *)
let print_file file = 
    let tab = from_file_to_list file
    in
    let rec print_line t =
        match t with
        | [] -> print_string "[end]\n";
        | [s] -> 
            print_string s;
            print_string "[end]\n";
        | h :: t -> 
            print_string h;
            print_char '\n';
            print_line t;
    in
    print_line tab