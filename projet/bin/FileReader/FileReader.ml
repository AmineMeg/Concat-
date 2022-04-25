let readFile =
    let in_file = open in "./exemples/fichier1.txt" in
    let line = input_line in_file in
    close_in in_file;
    print_endline line
;;