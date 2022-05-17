open FileTreatment;;
open GraphFile;;
(*open AST;;*)

let () =
    print_endline "Hello, World!";
    let file = "exemples/etape1/date_verbose"
    in
    print_file file;
    print_string "APRES DIJKSTRA \n";
    create_prog_from file ;;