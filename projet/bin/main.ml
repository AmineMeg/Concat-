open FileTreatment;; 
open GraphFile;;
open Graph;;

let () =
    print_endline "Hello, World!";
    let file = "projet/exemples/etape1/date_to_month_min"
    in
    print_file file;
    Graph.print (create_graph_from file);;
