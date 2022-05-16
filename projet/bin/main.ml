open FileTreatment;;
open GraphFile;;
open Graph;;
(*open AST;;*)

let () =
    print_endline "Hello, World!";
    let file = "exemples/etape1/example_enonce"
    in
    print_file file;
    Graph.print (create_graph_from file);;