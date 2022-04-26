open FileTreatment;; 
open GraphFile;;
open Graph;;

let () =
    print_endline "Hello, World!";;
    print_file "projet/exemples/etape1/date_to_month";;
    Graph.print (create_graph_from "projet/exemples/etape1/date_to_month");;
