let read_lines file process =
  let in_ch = open_in file in
  let rec read_line () =
    let line = try input_line in_ch with End_of_file -> exit 0
    in
        process line;
        read_line ();
in read_line ();;
(*ICI process = fonction qu'on voudra appliquer au lignes*)
let () =
    print_endline "Hello, World!";;
    read_lines "../../../bin/FileReader/exemples/fichier1.txt" print_endline;;
